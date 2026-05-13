#' Create a machinery context
#'
#' @param bundle Character. Bundle identifier.
#' @param connection A DBI connection object.
#' @param action_ctx Optional actionTypesR action context.
#' @export
mac_context <- function(bundle, connection, action_ctx = NULL) {
  ctx <- structure(
    list(
      bundle = bundle,
      con = connection,
      action_ctx = action_ctx,
      process_registry = new.env(parent = emptyenv())
    ),
    class = "mac_context"
  )
  mac_init_schema(ctx)
  ctx
}

mac_init_schema <- function(ctx) {
  DBI::dbExecute(ctx$con, "
    CREATE TABLE IF NOT EXISTS mac_processes (
      process_id     TEXT NOT NULL,
      name           TEXT NOT NULL,
      object_type_id TEXT NOT NULL,
      definition_json TEXT NOT NULL,
      version        INTEGER NOT NULL DEFAULT 1,
      created_at     TEXT NOT NULL,
      PRIMARY KEY (process_id)
    )
  ")
  DBI::dbExecute(ctx$con, "
    CREATE TABLE IF NOT EXISTS mac_instances (
      instance_id   TEXT NOT NULL,
      process_id    TEXT NOT NULL,
      object_key    TEXT NOT NULL,
      current_state TEXT NOT NULL,
      started_at    TEXT NOT NULL,
      updated_at    TEXT NOT NULL,
      completed_at  TEXT,
      metadata_json TEXT,
      PRIMARY KEY (instance_id),
      FOREIGN KEY (process_id) REFERENCES mac_processes(process_id)
    )
  ")
  DBI::dbExecute(ctx$con, "
    CREATE TABLE IF NOT EXISTS mac_transitions (
      transition_id TEXT NOT NULL,
      instance_id   TEXT NOT NULL,
      from_state    TEXT NOT NULL,
      to_state      TEXT NOT NULL,
      trigger_type  TEXT NOT NULL,
      trigger_ref   TEXT,
      triggered_by  TEXT,
      submission_id TEXT,
      triggered_at  TEXT NOT NULL,
      notes         TEXT,
      PRIMARY KEY (transition_id),
      FOREIGN KEY (instance_id) REFERENCES mac_instances(instance_id)
    )
  ")
  invisible(ctx)
}
