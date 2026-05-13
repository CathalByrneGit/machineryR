# Serialize process to JSON (functions stored as source strings)
process_to_json <- function(process) {
  p <- process
  p$transitions <- lapply(process$transitions, function(tr) {
    list(
      from = tr$from,
      to = tr$to,
      trigger = tr$trigger,
      guard_fn_src = fn_to_char(tr$guard_fn),
      on_enter_src = fn_to_char(tr$on_enter)
    )
  })
  # states to plain list
  p$states <- lapply(process$states, function(st) {
    list(name = st$name, display_name = st$display_name,
         terminal = st$terminal, sla_hours = st$sla_hours,
         description = st$description)
  })
  jsonlite::toJSON(p, auto_unbox = TRUE, null = "null")
}

# Deserialize process from JSON
json_to_process <- function(json_str) {
  p <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  # Rebuild states
  state_list <- stats::setNames(
    lapply(p$states, function(s) {
      mac_state(
        name = s$name,
        display_name = s$display_name %||% s$name,
        terminal = isTRUE(s$terminal),
        sla_hours = if (is.null(s$sla_hours) || identical(s$sla_hours, "NULL")) NULL else as.numeric(s$sla_hours),
        description = if (is.null(s$description) || identical(s$description, "NULL")) NULL else s$description
      )
    }),
    vapply(p$states, `[[`, character(1), "name")
  )

  # Rebuild transitions
  transitions <- lapply(p$transitions, function(tr) {
    trig_raw <- tr$trigger
    trigger <- switch(trig_raw$type,
      action    = mac_action_trigger(trig_raw$action_type_id),
      condition = mac_condition_trigger(trig_raw$sql_expr,
                    as.integer(trig_raw$check_interval_mins %||% 60L)),
      manual    = mac_manual_trigger(trig_raw$required_role),
      timeout   = mac_timeout_trigger(as.numeric(trig_raw$hours)),
      stop_mac("Unknown trigger type: ", trig_raw$type)
    )
    mac_transition(
      from     = tr$from,
      to       = tr$to,
      trigger  = trigger,
      guard_fn = char_to_fn(tr$guard_fn_src),
      on_enter = char_to_fn(tr$on_enter_src)
    )
  })

  mac_process(
    id             = p$id,
    name           = p$name,
    object_type_id = p$object_type_id,
    states         = state_list,
    transitions    = transitions,
    initial_state  = p$initial_state,
    table_name     = if (is.null(p$table_name) || identical(p$table_name, "NULL")) NULL else p$table_name,
    key_column     = if (is.null(p$key_column) || identical(p$key_column, "NULL")) NULL else p$key_column
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Register a process definition
#'
#' @param ctx A mac_context object.
#' @param process A mac_process object.
#' @export
mac_register <- function(ctx, process) {
  mac_validate_process(process)

  # Store in registry
  assign(process$id, process, envir = ctx$process_registry)

  json <- process_to_json(process)

  # Upsert
  existing <- DBI::dbGetQuery(
    ctx$con,
    "SELECT process_id FROM mac_processes WHERE process_id = ?",
    params = list(process$id)
  )
  if (nrow(existing) == 0) {
    DBI::dbExecute(
      ctx$con,
      "INSERT INTO mac_processes (process_id, name, object_type_id, definition_json, version, created_at)
       VALUES (?, ?, ?, ?, 1, ?)",
      params = list(process$id, process$name, process$object_type_id,
                    as.character(json), now_utc())
    )
  } else {
    DBI::dbExecute(
      ctx$con,
      "UPDATE mac_processes SET name = ?, definition_json = ?, version = version + 1
       WHERE process_id = ?",
      params = list(process$name, as.character(json), process$id)
    )
  }

  cli::cli_inform("Registered process '{process$id}' ({length(process$states)} states, {length(process$transitions)} transitions).")
  invisible(process)
}

#' Get a process definition
#'
#' @param ctx A mac_context object.
#' @param process_id Character. Process identifier.
#' @export
mac_get_process <- function(ctx, process_id) {
  # Check in-memory registry first
  if (exists(process_id, envir = ctx$process_registry, inherits = FALSE)) {
    return(get(process_id, envir = ctx$process_registry, inherits = FALSE))
  }
  # Fall back to DB
  row <- DBI::dbGetQuery(
    ctx$con,
    "SELECT definition_json FROM mac_processes WHERE process_id = ?",
    params = list(process_id)
  )
  if (nrow(row) == 0) stop_mac("Process '", process_id, "' not found.")
  json_to_process(row$definition_json[[1]])
}

#' List all registered processes
#'
#' @param ctx A mac_context object.
#' @export
mac_list_processes <- function(ctx) {
  DBI::dbGetQuery(ctx$con,
    "SELECT process_id, name, object_type_id, version, created_at FROM mac_processes ORDER BY created_at")
}
