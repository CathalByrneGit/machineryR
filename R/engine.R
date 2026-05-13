# Convert NULL to NA_character_ for RSQLite compatibility
null_to_na <- function(x) if (is.null(x)) NA_character_ else x

# Apply a transition to an instance (internal workhorse)
mac_apply_transition <- function(ctx, instance_id, from_state, to_state,
                                  trigger_type, trigger_ref = NULL,
                                  triggered_by = NULL, submission_id = NULL,
                                  notes = NULL, process = NULL) {
  now <- now_utc()
  tid <- new_id("tr_")

  # Record the transition (use NA instead of NULL for SQLite compatibility)
  DBI::dbExecute(ctx$con,
    "INSERT INTO mac_transitions
       (transition_id, instance_id, from_state, to_state, trigger_type,
        trigger_ref, triggered_by, submission_id, triggered_at, notes)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(tid, instance_id, from_state, to_state, trigger_type,
                  null_to_na(trigger_ref), null_to_na(triggered_by),
                  null_to_na(submission_id), now, null_to_na(notes))
  )

  # Determine if target state is terminal
  is_terminal <- FALSE
  if (!is.null(process)) {
    st <- process$states[[to_state]]
    if (!is.null(st)) is_terminal <- isTRUE(st$terminal)
  }

  completed_at_val <- if (is_terminal) now else NA_character_

  if (is_terminal) {
    DBI::dbExecute(ctx$con,
      "UPDATE mac_instances SET current_state = ?, updated_at = ?, completed_at = ?
       WHERE instance_id = ?",
      params = list(to_state, now, completed_at_val, instance_id)
    )
  } else {
    DBI::dbExecute(ctx$con,
      "UPDATE mac_instances SET current_state = ?, updated_at = ?
       WHERE instance_id = ?",
      params = list(to_state, now, instance_id)
    )
  }

  # Call on_enter if available
  if (!is.null(process)) {
    tr_def <- Filter(function(t) t$from == from_state && t$to == to_state,
                     process$transitions)
    if (length(tr_def) > 0 && !is.null(tr_def[[1]]$on_enter)) {
      instance <- mac_get_instance_raw(ctx, instance_id)
      tryCatch(
        tr_def[[1]]$on_enter(instance, list(ctx = ctx)),
        error = function(e) cli::cli_warn("on_enter error for {to_state}: {conditionMessage(e)}")
      )
    }
  }

  invisible(tid)
}

mac_get_instance_raw <- function(ctx, instance_id) {
  DBI::dbGetQuery(ctx$con,
    "SELECT * FROM mac_instances WHERE instance_id = ?",
    params = list(instance_id))
}

#' Check and apply condition-triggered transitions
#'
#' @param ctx A mac_context object.
#' @param process_id Character or NULL. If NULL, checks all processes.
#' @export
mac_check_conditions <- function(ctx, process_id = NULL) {
  results <- list()

  # Get candidate instances (non-terminal)
  query <- if (!is.null(process_id)) {
    DBI::dbGetQuery(ctx$con,
      "SELECT i.* FROM mac_instances i
       JOIN mac_processes p ON i.process_id = p.process_id
       WHERE i.process_id = ? AND i.completed_at IS NULL",
      params = list(process_id))
  } else {
    DBI::dbGetQuery(ctx$con,
      "SELECT i.* FROM mac_instances i WHERE i.completed_at IS NULL")
  }

  if (nrow(query) == 0) return(invisible(data.frame()))

  for (i in seq_len(nrow(query))) {
    inst <- as.list(query[i, ])
    proc <- tryCatch(mac_get_process(ctx, inst$process_id), error = function(e) NULL)
    if (is.null(proc)) next

    if (!is.null(process_id) && proc$id != process_id) next

    # Only process condition triggers from current state
    cond_transitions <- Filter(
      function(tr) tr$from == inst$current_state && tr$trigger$type == "condition",
      proc$transitions
    )
    if (length(cond_transitions) == 0) next

    for (tr in cond_transitions) {
      # Respect check_interval_mins
      interval <- tr$trigger$check_interval_mins %||% 60L
      last_check <- get_last_condition_check(ctx, inst$instance_id, tr$from, tr$to)
      if (!is.null(last_check)) {
        mins_since <- as.numeric(difftime(Sys.time(), last_check, units = "mins"))
        if (mins_since < interval) next
      }

      # Evaluate condition
      condition_met <- eval_condition(ctx, proc, inst$object_key, tr$trigger$sql_expr)
      record_condition_check(ctx, inst$instance_id, tr$from, tr$to)

      if (!condition_met) next

      # Check guard
      if (!is.null(tr$guard_fn)) {
        guarded <- tryCatch(
          isTRUE(tr$guard_fn(as.data.frame(query[i, ]), list(ctx = ctx))),
          error = function(e) FALSE
        )
        if (!guarded) next
      }

      tid <- mac_apply_transition(
        ctx, inst$instance_id, tr$from, tr$to,
        trigger_type = "condition", trigger_ref = tr$trigger$sql_expr,
        process = proc
      )
      results[[length(results) + 1]] <- list(
        instance_id = inst$instance_id, from = tr$from, to = tr$to,
        transition_id = tid
      )
      break  # one transition per instance per check cycle
    }
  }

  if (length(results) == 0) return(invisible(data.frame()))
  dplyr::bind_rows(lapply(results, as.data.frame))
}

eval_condition <- function(ctx, proc, object_key, sql_expr) {
  if (is.null(proc$table_name) || is.null(proc$key_column)) {
    cli::cli_warn("Condition trigger requires table_name and key_column on process '{proc$id}'")
    return(FALSE)
  }
  sql <- sprintf(
    "SELECT COUNT(*) AS n FROM %s WHERE %s = ? AND (%s)",
    proc$table_name, proc$key_column, sql_expr
  )
  res <- tryCatch(
    DBI::dbGetQuery(ctx$con, sql, params = list(object_key)),
    error = function(e) { cli::cli_warn("Condition eval error: {conditionMessage(e)}"); data.frame(n = 0) }
  )
  isTRUE(res$n[[1]] > 0)
}

# Simple in-memory condition check tracking (resets when session ends; acceptable for now)
.condition_check_cache <- new.env(parent = emptyenv())

record_condition_check <- function(ctx, instance_id, from, to) {
  key <- paste(instance_id, from, to, sep = "|")
  assign(key, Sys.time(), envir = .condition_check_cache)
}

get_last_condition_check <- function(ctx, instance_id, from, to) {
  key <- paste(instance_id, from, to, sep = "|")
  if (exists(key, envir = .condition_check_cache, inherits = FALSE)) {
    get(key, envir = .condition_check_cache, inherits = FALSE)
  } else {
    NULL
  }
}

#' Check and apply timeout-triggered transitions
#'
#' @param ctx A mac_context object.
#' @param process_id Character or NULL. If NULL, checks all processes.
#' @export
mac_check_timeouts <- function(ctx, process_id = NULL) {
  results <- list()

  query <- if (!is.null(process_id)) {
    DBI::dbGetQuery(ctx$con,
      "SELECT * FROM mac_instances WHERE process_id = ? AND completed_at IS NULL",
      params = list(process_id))
  } else {
    DBI::dbGetQuery(ctx$con,
      "SELECT * FROM mac_instances WHERE completed_at IS NULL")
  }

  if (nrow(query) == 0) return(invisible(data.frame()))

  for (i in seq_len(nrow(query))) {
    inst <- as.list(query[i, ])
    proc <- tryCatch(mac_get_process(ctx, inst$process_id), error = function(e) NULL)
    if (is.null(proc)) next

    timeout_transitions <- Filter(
      function(tr) tr$from == inst$current_state && tr$trigger$type == "timeout",
      proc$transitions
    )
    if (length(timeout_transitions) == 0) next

    # Time in current state
    hours_in_state <- calc_hours_in_state(ctx, inst$instance_id, inst$current_state, inst$updated_at)

    for (tr in timeout_transitions) {
      if (hours_in_state < tr$trigger$hours) next

      # Check guard
      if (!is.null(tr$guard_fn)) {
        guarded <- tryCatch(
          isTRUE(tr$guard_fn(as.data.frame(query[i, ]), list(ctx = ctx))),
          error = function(e) FALSE
        )
        if (!guarded) next
      }

      tid <- mac_apply_transition(
        ctx, inst$instance_id, tr$from, tr$to,
        trigger_type = "timeout",
        trigger_ref = paste0(tr$trigger$hours, "h"),
        process = proc
      )
      results[[length(results) + 1]] <- list(
        instance_id = inst$instance_id, from = tr$from, to = tr$to,
        transition_id = tid
      )
      break
    }
  }

  if (length(results) == 0) return(invisible(data.frame()))
  dplyr::bind_rows(lapply(results, as.data.frame))
}

calc_hours_in_state <- function(ctx, instance_id, current_state, updated_at) {
  # Find when instance entered current state (last transition to this state, or started_at)
  last_tr <- DBI::dbGetQuery(ctx$con,
    "SELECT triggered_at FROM mac_transitions
     WHERE instance_id = ? AND to_state = ?
     ORDER BY triggered_at DESC LIMIT 1",
    params = list(instance_id, current_state))

  entry_time_str <- if (nrow(last_tr) > 0) last_tr$triggered_at[[1]] else updated_at
  entry_time <- tryCatch(as.POSIXct(entry_time_str, tz = "UTC"), error = function(e) Sys.time())
  as.numeric(difftime(Sys.time(), entry_time, units = "hours"))
}
