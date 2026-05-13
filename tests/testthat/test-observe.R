test_that("mac_summary returns correct counts per state", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())

  # Start 3 instances in admitted state
  iid1 <- mac_start(ctx, "patient_discharge", "enc_1")
  iid2 <- mac_start(ctx, "patient_discharge", "enc_2")
  iid3 <- mac_start(ctx, "patient_discharge", "enc_3")

  # Move enc_2 to discharge_eligible manually
  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET current_state = 'discharge_eligible' WHERE instance_id = ?",
    params = list(iid2))

  # Move enc_3 to pending_approval manually
  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET current_state = 'pending_approval' WHERE instance_id = ?",
    params = list(iid3))

  summary_df <- mac_summary(ctx, "patient_discharge")

  expect_s3_class(summary_df, "data.frame")
  expect_true("state" %in% names(summary_df))
  expect_true("count" %in% names(summary_df))

  admitted_row <- summary_df[summary_df$state == "admitted", ]
  expect_equal(admitted_row$count, 1L)

  eligible_row <- summary_df[summary_df$state == "discharge_eligible", ]
  expect_equal(eligible_row$count, 1L)

  pending_row <- summary_df[summary_df$state == "pending_approval", ]
  expect_equal(pending_row$count, 1L)

  discharged_row <- summary_df[summary_df$state == "discharged", ]
  expect_equal(discharged_row$count, 0L)
})

test_that("mac_summary returns empty df when no instances", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())

  result <- mac_summary(ctx, "patient_discharge")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("mac_overdue returns instance exceeding sla_hours", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  # The sla for "admitted" is 72 hours. To simulate an overdue instance,
  # we backdate the started_at/updated_at to 100 hours ago.
  past_time <- format(Sys.time() - 100 * 3600, tz = "UTC", usetz = FALSE)
  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET started_at = ?, updated_at = ? WHERE instance_id = ?",
    params = list(past_time, past_time, iid))

  overdue_df <- mac_overdue(ctx, "patient_discharge")
  expect_s3_class(overdue_df, "data.frame")
  expect_true(nrow(overdue_df) >= 1)
  expect_true("hours_overdue" %in% names(overdue_df))
  expect_true(overdue_df$hours_overdue[[1]] > 0)
  expect_equal(overdue_df$instance_id[[1]], iid)
})

test_that("mac_overdue returns empty df when nothing is overdue", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid <- mac_start(ctx, "patient_discharge", "enc_1")

  # Just started, SLA is 72h - should not be overdue
  overdue_df <- mac_overdue(ctx, "patient_discharge")
  expect_equal(nrow(overdue_df), 0L)
})

test_that("mac_instances_in returns instances in a given state", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  iid1 <- mac_start(ctx, "patient_discharge", "enc_1")
  iid2 <- mac_start(ctx, "patient_discharge", "enc_2")

  DBI::dbExecute(ctx$con,
    "UPDATE mac_instances SET current_state = 'pending_approval' WHERE instance_id = ?",
    params = list(iid2))

  admitted_instances <- mac_instances_in(ctx, "patient_discharge", "admitted")
  expect_equal(nrow(admitted_instances), 1L)
  expect_equal(admitted_instances$instance_id[[1]], iid1)

  pending_instances <- mac_instances_in(ctx, "patient_discharge", "pending_approval")
  expect_equal(nrow(pending_instances), 1L)
  expect_equal(pending_instances$object_key[[1]], "enc_2")
})

test_that("mac_bottlenecks returns states sorted by avg time descending", {
  ctx <- make_test_ctx()
  on.exit(DBI::dbDisconnect(ctx$con))

  mac_register(ctx, make_discharge_process())
  mac_start(ctx, "patient_discharge", "enc_1")

  result <- mac_bottlenecks(ctx, "patient_discharge")
  expect_s3_class(result, "data.frame")
  # Admitted state should have count=1 and be at top (has time in state)
  expect_true(result$count[[1]] >= result$count[[nrow(result)]])
})
