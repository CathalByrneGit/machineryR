test_that("guard_fn returning FALSE blocks manual transition", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- mac_process(
    id = "guarded_process",
    name = "Guarded Process",
    object_type_id = "Encounter",
    initial_state = "start",
    states = list(
      mac_state("start"),
      mac_state("end", terminal = TRUE)
    ),
    transitions = list(
      mac_transition(
        from = "start", to = "end",
        trigger = mac_manual_trigger(),
        guard_fn = function(instance, env) FALSE  # always block
      )
    )
  )

  mac_register(ctx, proc)
  iid <- mac_start(ctx, "guarded_process", "enc_1")

  expect_error(mac_advance(ctx, iid, actor = "user1"),
               "blocked by guard")
  expect_equal(mac_state_of(ctx, iid), "start")
})

test_that("guard_fn returning TRUE allows manual transition", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- mac_process(
    id = "guarded_process2",
    name = "Guarded Process 2",
    object_type_id = "Encounter",
    initial_state = "start",
    states = list(
      mac_state("start"),
      mac_state("end", terminal = TRUE)
    ),
    transitions = list(
      mac_transition(
        from = "start", to = "end",
        trigger = mac_manual_trigger(),
        guard_fn = function(instance, env) TRUE  # always allow
      )
    )
  )

  mac_register(ctx, proc)
  iid <- mac_start(ctx, "guarded_process2", "enc_1")

  expect_no_error(mac_advance(ctx, iid, actor = "user1"))
  expect_equal(mac_state_of(ctx, iid), "end")
})

test_that("guard_fn blocking action trigger prevents transition", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- mac_process(
    id = "action_guarded",
    name = "Action Guarded",
    object_type_id = "Encounter",
    initial_state = "waiting",
    states = list(
      mac_state("waiting"),
      mac_state("done", terminal = TRUE)
    ),
    transitions = list(
      mac_transition(
        from = "waiting", to = "done",
        trigger = mac_action_trigger("DoAction"),
        guard_fn = function(instance, env) FALSE  # block
      )
    )
  )

  mac_register(ctx, proc)
  iid <- mac_start(ctx, "action_guarded", "enc_1")

  result <- mac_check_action_triggers(ctx, "DoAction", "enc_1")
  # guard blocked it, so no transitions
  expect_equal(nrow(result), 0L)
  expect_equal(mac_state_of(ctx, iid), "waiting")
})

test_that("guard_fn blocking condition trigger prevents transition", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- mac_process(
    id = "condition_guarded",
    name = "Condition Guarded",
    object_type_id = "Encounter",
    initial_state = "waiting",
    table_name = "encounters",
    key_column = "encounter_id",
    states = list(
      mac_state("waiting"),
      mac_state("done", terminal = TRUE)
    ),
    transitions = list(
      mac_transition(
        from = "waiting", to = "done",
        trigger = mac_condition_trigger("ready_for_discharge = 1", check_interval_mins = 0L),
        guard_fn = function(instance, env) FALSE  # always block
      )
    )
  )

  mac_register(ctx, proc)
  iid <- mac_start(ctx, "condition_guarded", "enc_1")

  # Mark encounter as ready (SQLite: 1 = TRUE)
  DBI::dbExecute(ctx$con,
    "UPDATE encounters SET ready_for_discharge = 1 WHERE encounter_id = 'enc_1'")

  result <- mac_check_conditions(ctx, "condition_guarded")
  # Guard should have blocked it
  expect_equal(mac_state_of(ctx, iid), "waiting")
})
