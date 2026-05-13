test_that("mac_start creates instance in admitted state", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  expect_type(iid, "character")
  expect_match(iid, "^inst_")
  expect_equal(mac_state_of(ctx, iid), "admitted")
})

test_that("mac_check_conditions transitions to discharge_eligible when ready", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  # Use interval 0 for immediate check; SQLite uses 1 for TRUE
  proc <- mac_process(
    id = "patient_discharge",
    name = "Patient Discharge Workflow",
    object_type_id = "Encounter",
    initial_state = "admitted",
    table_name = "encounters",
    key_column = "encounter_id",
    states = list(
      mac_state("admitted",           sla_hours = 72),
      mac_state("discharge_eligible", sla_hours = 24),
      mac_state("pending_approval",   sla_hours = 4),
      mac_state("discharged",         terminal = TRUE),
      mac_state("escalated",          terminal = TRUE)
    ),
    transitions = list(
      mac_transition(
        from = "admitted", to = "discharge_eligible",
        trigger = mac_condition_trigger("ready_for_discharge = 1", check_interval_mins = 0L)
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
  mac_register(ctx, proc)
  iid <- mac_start(ctx, "patient_discharge", "enc_1")
  expect_equal(mac_state_of(ctx, iid), "admitted")

  # Not ready yet - condition should not fire
  result <- mac_check_conditions(ctx, "patient_discharge")
  expect_equal(mac_state_of(ctx, iid), "admitted")

  # Now mark as ready (SQLite: 1 = TRUE)
  DBI::dbExecute(ctx$con,
    "UPDATE encounters SET ready_for_discharge = 1 WHERE encounter_id = 'enc_1'")

  # Clear condition check cache so interval check is bypassed
  rm(list = ls(envir = machineryR:::.condition_check_cache),
     envir = machineryR:::.condition_check_cache)

  result2 <- mac_check_conditions(ctx, "patient_discharge")
  expect_equal(mac_state_of(ctx, iid), "discharge_eligible")
  expect_true(nrow(result2) > 0)
})

test_that("mac_advance errors when no manual trigger from current state", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  # admitted state has only condition trigger, not manual
  expect_error(mac_advance(ctx, iid, actor = "nurse_1"),
               "No manual transitions")
})

test_that("mac_check_action_triggers with InitiateDischarge moves to pending_approval", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  # Manually set to discharge_eligible (bypass condition for speed)
  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET current_state = 'discharge_eligible' WHERE instance_id = ?",
    params = list(iid))

  result <- mac_check_action_triggers(ctx, "InitiateDischarge", "enc_1")
  expect_equal(mac_state_of(ctx, iid), "pending_approval")
  expect_equal(nrow(result), 1L)
  expect_equal(result$from[[1]], "discharge_eligible")
  expect_equal(result$to[[1]], "pending_approval")
})

test_that("mac_advance with approver moves to discharged", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  # Move to pending_approval
  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET current_state = 'pending_approval' WHERE instance_id = ?",
    params = list(iid))

  # auditR not installed so role check is skipped - advance should succeed
  tid <- mac_advance(ctx, iid, actor = "dr_jones", to_state = "discharged")
  expect_equal(mac_state_of(ctx, iid), "discharged")
  expect_type(tid, "character")

  # Check instance is marked completed
  row <- DBI::dbGetQuery(ctx$con,
    "SELECT completed_at FROM mac_instances WHERE instance_id = ?",
    params = list(iid))
  expect_false(is.na(row$completed_at[[1]]))
})

test_that("mac_check_timeouts fires when timeout expires (0 hours)", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  # Build process with 0-hour timeout for immediate firing
  proc <- mac_process(
    id = "timeout_test",
    name = "Timeout Test",
    object_type_id = "Encounter",
    initial_state = "pending",
    states = list(
      mac_state("pending", sla_hours = 1),
      mac_state("timed_out", terminal = TRUE)
    ),
    transitions = list(
      mac_transition(
        from = "pending", to = "timed_out",
        trigger = mac_timeout_trigger(hours = 0)
      )
    )
  )
  mac_register(ctx, proc)
  iid <- mac_start(ctx, "timeout_test", "enc_1")

  expect_equal(mac_state_of(ctx, iid), "pending")

  result <- mac_check_timeouts(ctx, "timeout_test")
  expect_equal(mac_state_of(ctx, iid), "timed_out")
  expect_true(nrow(result) >= 1)
})

test_that("mac_advance errors on completed instance", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET current_state = 'pending_approval' WHERE instance_id = ?",
    params = list(iid))

  mac_advance(ctx, iid, actor = "dr_jones", to_state = "discharged")

  expect_error(mac_advance(ctx, iid, actor = "dr_jones", to_state = "discharged"),
               "already completed")
})

test_that("mac_history returns transition records", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET current_state = 'pending_approval' WHERE instance_id = ?",
    params = list(iid))
  mac_advance(ctx, iid, actor = "dr_jones", to_state = "discharged")

  hist <- mac_history(ctx, iid)
  expect_s3_class(hist, "data.frame")
  expect_true(nrow(hist) >= 1)
  expect_true("from_state" %in% names(hist))
  expect_true("to_state" %in% names(hist))
})
