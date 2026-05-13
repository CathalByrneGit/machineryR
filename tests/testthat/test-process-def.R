test_that("valid process registers successfully", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- make_discharge_process()
  expect_no_error(mac_register(ctx, proc))

  procs <- mac_list_processes(ctx)
  expect_equal(nrow(procs), 1L)
  expect_equal(procs$process_id[[1]], "patient_discharge")
  expect_equal(procs$name[[1]], "Patient Discharge Workflow")
})

test_that("process with non-existent initial_state aborts", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- mac_process(
    id = "bad_initial",
    name = "Bad",
    object_type_id = "X",
    initial_state = "nonexistent",
    states = list(
      mac_state("real_state", terminal = TRUE)
    ),
    transitions = list()
  )
  expect_error(mac_register(ctx, proc), "initial_state")
})

test_that("process with undefined state in transition aborts", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- mac_process(
    id = "bad_transition",
    name = "Bad",
    object_type_id = "X",
    initial_state = "start",
    states = list(
      mac_state("start"),
      mac_state("end", terminal = TRUE)
    ),
    transitions = list(
      mac_transition("start", "ghost_state", mac_manual_trigger())
    )
  )
  expect_error(mac_register(ctx, proc), "undefined state")
})

test_that("process with terminal state having outgoing transition aborts", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- mac_process(
    id = "bad_terminal",
    name = "Bad",
    object_type_id = "X",
    initial_state = "start",
    states = list(
      mac_state("start"),
      mac_state("done", terminal = TRUE),
      mac_state("extra", terminal = TRUE)
    ),
    transitions = list(
      mac_transition("start", "done", mac_manual_trigger()),
      mac_transition("done", "extra", mac_manual_trigger())
    )
  )
  expect_error(mac_register(ctx, proc), "terminal state")
})

test_that("process with orphan/unreachable state aborts", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- mac_process(
    id = "unreachable",
    name = "Unreachable",
    object_type_id = "X",
    initial_state = "start",
    states = list(
      mac_state("start"),
      mac_state("middle"),
      mac_state("end", terminal = TRUE),
      mac_state("orphan", terminal = TRUE)
    ),
    transitions = list(
      mac_transition("start", "middle", mac_manual_trigger()),
      mac_transition("middle", "end", mac_manual_trigger())
    )
  )
  expect_error(mac_register(ctx, proc), "unreachable")
})

test_that("process with non-terminal dead-end state aborts", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- mac_process(
    id = "dead_end",
    name = "DeadEnd",
    object_type_id = "X",
    initial_state = "start",
    states = list(
      mac_state("start"),
      mac_state("stuck")   # non-terminal, no outgoing transitions
    ),
    transitions = list(
      mac_transition("start", "stuck", mac_manual_trigger())
    )
  )
  expect_error(mac_register(ctx, proc), "dead end")
})

test_that("mac_get_process retrieves from DB after registry cleared", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- make_discharge_process()
  mac_register(ctx, proc)

  # Clear registry to force DB lookup
  rm(list = ls(envir = ctx$process_registry), envir = ctx$process_registry)

  retrieved <- mac_get_process(ctx, "patient_discharge")
  expect_equal(retrieved$id, "patient_discharge")
  expect_equal(length(retrieved$states), 5L)
  expect_equal(length(retrieved$transitions), 4L)
})

test_that("mac_register updates version on re-registration", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  proc <- make_discharge_process()
  mac_register(ctx, proc)
  mac_register(ctx, proc)  # second registration

  procs <- mac_list_processes(ctx)
  expect_equal(procs$version[[1]], 2L)
})
