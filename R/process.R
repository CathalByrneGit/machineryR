#' Define a state
#'
#' @param name Character. State identifier.
#' @param display_name Character. Human-readable name.
#' @param terminal Logical. Whether this is a terminal state.
#' @param sla_hours Numeric or NULL. SLA in hours.
#' @param description Character or NULL. Description of the state.
#' @export
mac_state <- function(name, display_name = name, terminal = FALSE,
                       sla_hours = NULL, description = NULL) {
  structure(
    list(name = name, display_name = display_name, terminal = terminal,
         sla_hours = sla_hours, description = description),
    class = "mac_state"
  )
}

#' Action trigger
#'
#' @param action_type_id Character. Action type identifier.
#' @export
mac_action_trigger <- function(action_type_id) {
  structure(list(type = "action", action_type_id = action_type_id),
            class = "mac_trigger")
}

#' Condition trigger
#'
#' @param sql_expr Character. SQL expression to evaluate.
#' @param check_interval_mins Integer. How often to check in minutes.
#' @export
mac_condition_trigger <- function(sql_expr, check_interval_mins = 60L) {
  structure(list(type = "condition", sql_expr = sql_expr,
                 check_interval_mins = as.integer(check_interval_mins)),
            class = "mac_trigger")
}

#' Manual trigger
#'
#' @param required_role Character or NULL. Role required to advance.
#' @export
mac_manual_trigger <- function(required_role = NULL) {
  structure(list(type = "manual", required_role = required_role),
            class = "mac_trigger")
}

#' Timeout trigger
#'
#' @param hours Numeric. Hours until timeout fires.
#' @export
mac_timeout_trigger <- function(hours) {
  structure(list(type = "timeout", hours = hours), class = "mac_trigger")
}

#' Define a transition
#'
#' @param from Character. Source state name.
#' @param to Character. Target state name.
#' @param trigger A mac_trigger object.
#' @param guard_fn Function or NULL. Guard function that returns TRUE/FALSE.
#' @param on_enter Function or NULL. Callback fired on state entry.
#' @export
mac_transition <- function(from, to, trigger, guard_fn = NULL, on_enter = NULL) {
  structure(
    list(from = from, to = to, trigger = trigger,
         guard_fn = guard_fn, on_enter = on_enter),
    class = "mac_transition"
  )
}

#' Define a workflow process
#'
#' @param id Character. Stable process identifier.
#' @param name Character. Human-readable name.
#' @param object_type_id Character. Ontology object type.
#' @param states Named list of mac_state() objects.
#' @param transitions List of mac_transition() objects.
#' @param initial_state Character. Starting state name.
#' @param table_name Character or NULL. DB table for condition evaluation.
#' @param key_column Character or NULL. PK column in table_name.
#' @export
mac_process <- function(id, name, object_type_id, states, transitions,
                         initial_state, table_name = NULL, key_column = NULL) {
  # Ensure states is a named list keyed by state name
  if (!is.null(names(states))) {
    # already named
    state_list <- states
  } else {
    state_list <- stats::setNames(states, vapply(states, `[[`, character(1), "name"))
  }

  structure(
    list(
      id = id, name = name, object_type_id = object_type_id,
      states = state_list, transitions = transitions,
      initial_state = initial_state,
      table_name = table_name, key_column = key_column
    ),
    class = "mac_process"
  )
}
