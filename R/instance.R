#' Start a new process instance for an object
#'
#' @param ctx mac_context object
#' @param process_id Character. Process identifier.
#' @param object_key Character. Primary key of the tracked object.
#' @param metadata Named list or NULL. Additional metadata stored as JSON.
#' @return Character. The new instance_id.
#' @export
mac_start <- function(ctx, process_id, object_key, metadata = NULL) {
  proc <- mac_get_process(ctx, process_id)

  iid <- new_id("inst_")
  now <- now_utc()
  meta_json <- if (!is.null(metadata)) jsonlite::toJSON(metadata, auto_unbox = TRUE) else NA_character_

  DBI::dbExecute(ctx$con,
    "INSERT INTO mac_instances
       (instance_id, process_id, object_key, current_state, started_at, updated_at, metadata_json)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(iid, process_id, object_key, proc$initial_state, now, now, meta_json)
  )

  # Call on_enter for initial state if any transition into it (unlikely, but check anyway)
  init_on_enter <- get_initial_on_enter(proc)
  if (!is.null(init_on_enter)) {
    instance <- mac_get_instance_raw(ctx, iid)
    tryCatch(
      init_on_enter(instance, list(ctx = ctx)),
      error = function(e) cli::cli_warn("on_enter error for initial state: {conditionMessage(e)}")
    )
  }

  cli::cli_inform("Started instance '{iid}' for object '{object_key}' in process '{process_id}' (state: {proc$initial_state}).")
  iid
}

get_initial_on_enter <- function(proc) {
  # Find a transition to the initial state (as 'to') with on_enter defined
  # In practice, this is rarely used; return NULL
  NULL
}

#' Manually advance an instance to the next state
#'
#' @param ctx mac_context object
#' @param instance_id Character. Instance identifier.
#' @param actor Character. Who is performing the advance.
#' @param to_state Character or NULL. Required if multiple manual transitions are available.
#' @param notes Character or NULL.
#' @export
mac_advance <- function(ctx, instance_id, actor, to_state = NULL, notes = NULL) {
  inst <- mac_get_instance_raw(ctx, instance_id)
  if (nrow(inst) == 0) stop_mac("Instance '", instance_id, "' not found.")
  inst <- as.list(inst[1, ])

  if (!is.null(inst$completed_at) && !is.na(inst$completed_at)) {
    stop_mac("Instance '", instance_id, "' is already completed.")
  }

  proc <- mac_get_process(ctx, inst$process_id)

  # Find manual transitions from current state
  manual_trs <- Filter(
    function(tr) tr$from == inst$current_state && tr$trigger$type == "manual",
    proc$transitions
  )

  if (length(manual_trs) == 0) {
    stop_mac("No manual transitions available from state '", inst$current_state, "'.")
  }

  if (!is.null(to_state)) {
    manual_trs <- Filter(function(tr) tr$to == to_state, manual_trs)
    if (length(manual_trs) == 0) {
      stop_mac("No manual transition to '", to_state, "' from state '", inst$current_state, "'.")
    }
  } else if (length(manual_trs) > 1) {
    targets <- paste(vapply(manual_trs, `[[`, character(1), "to"), collapse = ", ")
    stop_mac("Ambiguous transition from '", inst$current_state,
             "': multiple manual transitions available (", targets,
             "). Specify to_state.")
  }

  tr <- manual_trs[[1]]

  # Check role
  required_role <- tr$trigger$required_role
  if (!is.null(required_role) && !is.na(required_role) && !identical(required_role, "NULL")) {
    if (requireNamespace("auditR", quietly = TRUE)) {
      if (!auditR::has_role(actor, required_role)) {
        stop_mac("Actor '", actor, "' does not have required role '", required_role, "'.")
      }
    }
    # If auditR not available, skip role check (graceful degradation)
  }

  # Check guard
  if (!is.null(tr$guard_fn)) {
    instance_df <- mac_get_instance_raw(ctx, instance_id)
    guarded <- tryCatch(
      isTRUE(tr$guard_fn(instance_df, list(ctx = ctx))),
      error = function(e) {
        stop_mac("guard_fn error: ", conditionMessage(e))
      }
    )
    if (!guarded) {
      stop_mac("Transition from '", tr$from, "' to '", tr$to,
               "' blocked by guard function.")
    }
  }

  tid <- mac_apply_transition(
    ctx, instance_id, tr$from, tr$to,
    trigger_type = "manual", triggered_by = actor,
    notes = notes, process = proc
  )

  cli::cli_inform("Advanced instance '{instance_id}': {tr$from} -> {tr$to} (by {actor}).")
  invisible(tid)
}

#' Get current state of an instance
#'
#' @param ctx A mac_context object.
#' @param instance_id Character. Instance identifier.
#' @export
mac_state_of <- function(ctx, instance_id) {
  row <- DBI::dbGetQuery(ctx$con,
    "SELECT current_state FROM mac_instances WHERE instance_id = ?",
    params = list(instance_id))
  if (nrow(row) == 0) stop_mac("Instance '", instance_id, "' not found.")
  row$current_state[[1]]
}

#' Get full transition history of an instance
#'
#' @param ctx A mac_context object.
#' @param instance_id Character. Instance identifier.
#' @export
mac_history <- function(ctx, instance_id) {
  DBI::dbGetQuery(ctx$con,
    "SELECT * FROM mac_transitions WHERE instance_id = ? ORDER BY triggered_at",
    params = list(instance_id))
}
