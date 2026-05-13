test_that("on_enter is called when state is entered via manual transition", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  # Use an environment to capture side-effect
  env <- new.env(parent = emptyenv())
  env$fired <- FALSE
  env$fired_state <- NULL

  on_enter_fn <- function(instance, context) {
    env$fired <- TRUE
    env$fired_state <- instance$current_state[[1]]
  }

  proc <- mac_process(
    id = "on_enter_test",
    name = "On Enter Test",
    object_type_id = "Encounter",
    initial_state = "start",
    states = list(
      mac_state("start"),
      mac_state("finish", terminal = TRUE)
    ),
    transitions = list(
      mac_transition(
        from = "start", to = "finish",
        trigger = mac_manual_trigger(),
        on_enter = on_enter_fn
      )
    )
  )

  mac_register(ctx, proc)
  iid <- mac_start(ctx, "on_enter_test", "enc_1")

  expect_false(env$fired)
  mac_advance(ctx, iid, actor = "user1")
  expect_true(env$fired)
  expect_equal(env$fired_state, "finish")
})

test_that("on_enter is called when state is entered via action trigger", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  env <- new.env(parent = emptyenv())
  env$call_count <- 0L

  on_enter_fn <- function(instance, context) {
    env$call_count <- env$call_count + 1L
  }

  proc <- mac_process(
    id = "action_on_enter",
    name = "Action On Enter",
    object_type_id = "Encounter",
    initial_state = "waiting",
    states = list(
      mac_state("waiting"),
      mac_state("processing", terminal = TRUE)
    ),
    transitions = list(
      mac_transition(
        from = "waiting", to = "processing",
        trigger = mac_action_trigger("StartProcessing"),
        on_enter = on_enter_fn
      )
    )
  )

  mac_register(ctx, proc)
  iid <- mac_start(ctx, "action_on_enter", "enc_1")

  expect_equal(env$call_count, 0L)
  mac_check_action_triggers(ctx, "StartProcessing", "enc_1")
  expect_equal(env$call_count, 1L)
  expect_equal(mac_state_of(ctx, iid), "processing")
})

test_that("on_enter error is caught and does not abort transition", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  bad_on_enter <- function(instance, context) {
    stop("Intentional on_enter failure!")
  }

  proc <- mac_process(
    id = "bad_on_enter",
    name = "Bad On Enter",
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
        on_enter = bad_on_enter
      )
    )
  )

  mac_register(ctx, proc)
  iid <- mac_start(ctx, "bad_on_enter", "enc_1")

  # Should warn but not error - transition still completes
  # Suppress the warning from on_enter, verify state changes
  suppressWarnings(
    mac_advance(ctx, iid, actor = "user1")
  )
  # The transition still happened despite on_enter error
  expect_equal(mac_state_of(ctx, iid), "end")
})
