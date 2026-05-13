#' Validate a process definition
#' @keywords internal
mac_validate_process <- function(process) {
  state_names <- names(process$states)

  # Duplicate state names
  if (anyDuplicated(state_names) > 0) {
    stop_mac("Process '", process$id, "': duplicate state names detected.")
  }

  # initial_state exists
  if (!process$initial_state %in% state_names) {
    stop_mac("Process '", process$id, "': initial_state '", process$initial_state,
             "' is not a defined state.")
  }

  # All transition from/to states exist
  for (tr in process$transitions) {
    if (!tr$from %in% state_names) {
      stop_mac("Process '", process$id, "': transition from='", tr$from,
               "' references an undefined state.")
    }
    if (!tr$to %in% state_names) {
      stop_mac("Process '", process$id, "': transition to='", tr$to,
               "' references an undefined state.")
    }
    # Terminal state has no outgoing transition
    if (isTRUE(process$states[[tr$from]]$terminal)) {
      stop_mac("Process '", process$id, "': terminal state '", tr$from,
               "' cannot have outgoing transitions.")
    }
  }

  # Non-terminal states without outgoing transitions (dead ends)
  for (sname in state_names) {
    st <- process$states[[sname]]
    if (isTRUE(st$terminal)) next
    has_out <- any(vapply(process$transitions, function(tr) tr$from == sname, logical(1)))
    if (!has_out) {
      stop_mac("Process '", process$id, "': non-terminal state '", sname,
               "' has no outgoing transitions (dead end). Mark it terminal or add a transition.")
    }
  }

  # Reachability from initial_state (BFS)
  reachable <- character(0)
  queue <- process$initial_state
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    if (current %in% reachable) next
    reachable <- c(reachable, current)
    nexts <- vapply(
      Filter(function(tr) tr$from == current, process$transitions),
      function(tr) tr$to, character(1)
    )
    queue <- c(queue, setdiff(nexts, reachable))
  }
  unreachable <- setdiff(state_names, reachable)
  if (length(unreachable) > 0) {
    stop_mac("Process '", process$id, "': states unreachable from initial_state '",
             process$initial_state, "': ", paste(unreachable, collapse = ", "))
  }

  invisible(TRUE)
}
