#' Summarise process state distribution
#'
#' @param ctx A mac_context object.
#' @param process_id Character. Process identifier.
#' @export
mac_summary <- function(ctx, process_id) {
  proc <- mac_get_process(ctx, process_id)

  # Get all instances for this process
  instances <- DBI::dbGetQuery(ctx$con,
    "SELECT i.instance_id, i.current_state, i.updated_at, i.started_at, i.completed_at
     FROM mac_instances i
     WHERE i.process_id = ?",
    params = list(process_id))

  if (nrow(instances) == 0) {
    return(data.frame(
      state = character(), count = integer(),
      avg_hours_in_state = numeric(), overdue_count = integer()
    ))
  }

  # Compute per-instance time in current state
  rows <- lapply(seq_len(nrow(instances)), function(i) {
    inst <- as.list(instances[i, ])
    hrs <- calc_hours_in_state(ctx, inst$instance_id, inst$current_state, inst$updated_at)

    st <- proc$states[[inst$current_state]]
    sla <- if (!is.null(st)) st$sla_hours else NULL
    overdue <- !is.null(sla) && !is.na(sla) && hrs > sla

    list(state = inst$current_state, hours = hrs, overdue = overdue)
  })

  states_all <- names(proc$states)

  result <- do.call(rbind, lapply(states_all, function(s) {
    state_rows <- Filter(function(r) r$state == s, rows)
    count <- length(state_rows)
    avg_hrs <- if (count > 0) mean(vapply(state_rows, `[[`, numeric(1), "hours")) else 0
    overdue_count <- sum(vapply(state_rows, `[[`, logical(1), "overdue"))
    data.frame(state = s, count = count, avg_hours_in_state = round(avg_hrs, 2),
               overdue_count = overdue_count, stringsAsFactors = FALSE)
  }))

  result
}

#' Get instances by state
#'
#' @param ctx A mac_context object.
#' @param process_id Character. Process identifier.
#' @param state Character. State name to filter by.
#' @export
mac_instances_in <- function(ctx, process_id, state) {
  instances <- DBI::dbGetQuery(ctx$con,
    "SELECT instance_id, object_key, updated_at FROM mac_instances
     WHERE process_id = ? AND current_state = ?",
    params = list(process_id, state))

  if (nrow(instances) == 0) {
    return(data.frame(instance_id = character(), object_key = character(),
                      entered_state_at = character(), hours_in_state = numeric()))
  }

  result <- do.call(rbind, lapply(seq_len(nrow(instances)), function(i) {
    inst <- as.list(instances[i, ])
    hrs <- calc_hours_in_state(ctx, inst$instance_id, state, inst$updated_at)

    # Find when this state was entered
    last_tr <- DBI::dbGetQuery(ctx$con,
      "SELECT triggered_at FROM mac_transitions
       WHERE instance_id = ? AND to_state = ?
       ORDER BY triggered_at DESC LIMIT 1",
      params = list(inst$instance_id, state))

    entered <- if (nrow(last_tr) > 0) last_tr$triggered_at[[1]] else inst$updated_at

    data.frame(instance_id = inst$instance_id, object_key = inst$object_key,
               entered_state_at = entered, hours_in_state = round(hrs, 2),
               stringsAsFactors = FALSE)
  }))

  result
}

#' Get bottleneck analysis
#'
#' @param ctx A mac_context object.
#' @param process_id Character. Process identifier.
#' @export
mac_bottlenecks <- function(ctx, process_id) {
  summary_df <- mac_summary(ctx, process_id)
  summary_df[order(-summary_df$avg_hours_in_state, -summary_df$overdue_count), ]
}

#' Get overdue instances
#'
#' @param ctx A mac_context object.
#' @param process_id Character or NULL. If NULL, checks all processes.
#' @export
mac_overdue <- function(ctx, process_id = NULL) {
  query <- if (!is.null(process_id)) {
    DBI::dbGetQuery(ctx$con,
      "SELECT * FROM mac_instances WHERE process_id = ? AND completed_at IS NULL",
      params = list(process_id))
  } else {
    DBI::dbGetQuery(ctx$con,
      "SELECT * FROM mac_instances WHERE completed_at IS NULL")
  }

  if (nrow(query) == 0) {
    return(data.frame(instance_id = character(), process_id = character(),
                      object_key = character(), current_state = character(),
                      hours_in_state = numeric(), sla_hours = numeric(),
                      hours_overdue = numeric()))
  }

  results <- list()
  for (i in seq_len(nrow(query))) {
    inst <- as.list(query[i, ])
    proc <- tryCatch(mac_get_process(ctx, inst$process_id), error = function(e) NULL)
    if (is.null(proc)) next

    st <- proc$states[[inst$current_state]]
    if (is.null(st) || is.null(st$sla_hours) || is.na(st$sla_hours)) next

    hrs <- calc_hours_in_state(ctx, inst$instance_id, inst$current_state, inst$updated_at)
    if (hrs <= st$sla_hours) next

    results[[length(results) + 1]] <- data.frame(
      instance_id = inst$instance_id,
      process_id = inst$process_id,
      object_key = inst$object_key,
      current_state = inst$current_state,
      hours_in_state = round(hrs, 2),
      sla_hours = st$sla_hours,
      hours_overdue = round(hrs - st$sla_hours, 2),
      stringsAsFactors = FALSE
    )
  }

  if (length(results) == 0) {
    return(data.frame(instance_id = character(), process_id = character(),
                      object_key = character(), current_state = character(),
                      hours_in_state = numeric(), sla_hours = numeric(),
                      hours_overdue = numeric()))
  }

  dplyr::bind_rows(results)
}
