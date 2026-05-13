library(machineryR)
library(RSQLite)
library(DBI)

# Build the canonical patient discharge process for testing
make_discharge_process <- function(table_name = "encounters", key_column = "encounter_id") {
  mac_process(
    id = "patient_discharge",
    name = "Patient Discharge Workflow",
    object_type_id = "Encounter",
    initial_state = "admitted",
    table_name = table_name,
    key_column = key_column,
    states = list(
      mac_state("admitted",           display_name = "Admitted",          sla_hours = 72),
      mac_state("discharge_eligible", display_name = "Discharge Eligible", sla_hours = 24),
      mac_state("pending_approval",   display_name = "Pending Approval",   sla_hours = 4),
      mac_state("discharged",         display_name = "Discharged",         terminal = TRUE),
      mac_state("escalated",          display_name = "Escalated",          terminal = TRUE)
    ),
    transitions = list(
      mac_transition(
        from = "admitted", to = "discharge_eligible",
        trigger = mac_condition_trigger("ready_for_discharge = 1", check_interval_mins = 1L)
      ),
      mac_transition(
        from = "discharge_eligible", to = "pending_approval",
        trigger = mac_action_trigger("InitiateDischarge")
      ),
      mac_transition(
        from = "pending_approval", to = "discharged",
        trigger = mac_manual_trigger(required_role = "discharge_approver")
      ),
      mac_transition(
        from = "pending_approval", to = "escalated",
        trigger = mac_timeout_trigger(hours = 4)
      )
    )
  )
}

make_test_ctx <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # Create mock encounters table
  DBI::dbExecute(con, "
    CREATE TABLE encounters (
      encounter_id TEXT PRIMARY KEY,
      patient_name TEXT,
      ready_for_discharge INTEGER DEFAULT 0
    )
  ")
  for (i in 1:5) {
    DBI::dbExecute(con,
      "INSERT INTO encounters VALUES (?, ?, 0)",
      params = list(paste0("enc_", i), paste0("Patient ", i))
    )
  }
  ctx <- mac_context("test_bundle", con)
  ctx
}
