test_that("mac_check_action_triggers transitions instance on matching action", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  # Manually advance to discharge_eligible (skipping condition trigger for speed)
  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET current_state = 'discharge_eligible' WHERE instance_id = ?",
    params = list(iid))

  # Simulate action submission
  result <- mac_check_action_triggers(ctx, "InitiateDischarge", "enc_1", submission_id = "sub_001")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  expect_equal(result$from[[1]], "discharge_eligible")
  expect_equal(result$to[[1]], "pending_approval")
  expect_equal(mac_state_of(ctx, iid), "pending_approval")

  # Verify the transition record has submission_id
  hist <- mac_history(ctx, iid)
  expect_true(nrow(hist) >= 1)
  action_rows <- hist[hist$trigger_type == "action", ]
  expect_true(nrow(action_rows) >= 1)
  expect_equal(action_rows$submission_id[[1]], "sub_001")
  expect_equal(action_rows$trigger_ref[[1]], "InitiateDischarge")
})

test_that("mac_check_action_triggers with wrong action_type_id does nothing", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET current_state = 'discharge_eligible' WHERE instance_id = ?",
    params = list(iid))

  result <- mac_check_action_triggers(ctx, "WrongAction", "enc_1")
  expect_equal(nrow(result), 0L)
  expect_equal(mac_state_of(ctx, iid), "discharge_eligible")
})

test_that("mac_check_action_triggers with wrong object_key does nothing", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET current_state = 'discharge_eligible' WHERE instance_id = ?",
    params = list(iid))

  result <- mac_check_action_triggers(ctx, "InitiateDischarge", "enc_999")
  expect_equal(nrow(result), 0L)
  expect_equal(mac_state_of(ctx, iid), "discharge_eligible")
})

test_that("mac_check_action_triggers handles multiple target_ids", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid1 <- mac_start(ctx, "patient_discharge", "enc_1")
  iid2 <- mac_start(ctx, "patient_discharge", "enc_2")
  iid3 <- mac_start(ctx, "patient_discharge", "enc_3")  # stays in admitted

  # Advance enc_1 and enc_2 to discharge_eligible
  for (iid in c(iid1, iid2)) {
    DBI::dbExecute(ctx$con,
      "UPDATE mac_instances SET current_state = 'discharge_eligible' WHERE instance_id = ?",
      params = list(iid))
  }

  result <- mac_check_action_triggers(ctx, "InitiateDischarge", c("enc_1", "enc_2", "enc_3"))

  expect_equal(nrow(result), 2L)
  expect_equal(mac_state_of(ctx, iid1), "pending_approval")
  expect_equal(mac_state_of(ctx, iid2), "pending_approval")
  expect_equal(mac_state_of(ctx, iid3), "admitted")  # not affected
})

test_that("mac_wire_actions warns when actionTypesR not available", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())

  # Fake action_ctx
  action_ctx <- list(name = "fake_ctx")

  # Should warn about missing actionTypesR
  expect_warning(
    mac_wire_actions(ctx, action_ctx),
    "actionTypesR"
  )
})

test_that("full workflow: admitted -> discharge_eligible -> pending_approval -> discharged", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

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

  # Condition trigger: mark ready
  DBI::dbExecute(ctx$con,
    "UPDATE encounters SET ready_for_discharge = 1 WHERE encounter_id = 'enc_1'")
  rm(list = ls(envir = machineryR:::.condition_check_cache),
     envir = machineryR:::.condition_check_cache)

  mac_check_conditions(ctx, "patient_discharge")
  expect_equal(mac_state_of(ctx, iid), "discharge_eligible")

  # Action trigger
  mac_check_action_triggers(ctx, "InitiateDischarge", "enc_1")
  expect_equal(mac_state_of(ctx, iid), "pending_approval")

  # Manual advance
  mac_advance(ctx, iid, actor = "dr_jones", to_state = "discharged")
  expect_equal(mac_state_of(ctx, iid), "discharged")

  # Check history has all transitions
  hist <- mac_history(ctx, iid)
  expect_equal(nrow(hist), 3L)
  expect_equal(hist$trigger_type, c("condition", "action", "manual"))
})
