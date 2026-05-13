#' Wire actionTypesR action submissions to process transitions
#'
#' @param mac_ctx A mac_context object.
#' @param action_ctx An actionTypesR action context.
#' @export
mac_wire_actions <- function(mac_ctx, action_ctx) {
  # We add a hook to action_ctx that fires mac_check_action_triggers after each submission
  # actionTypesR's action_ctx has a hooks list; we append to its post_submit hooks.
  # If actionTypesR is not available, warn and return action_ctx unchanged.
  if (!requireNamespace("actionTypesR", quietly = TRUE)) {
    cli::cli_warn("actionTypesR not available; mac_wire_actions() has no effect.")
    return(action_ctx)
  }

  hook_fn <- function(action_type_id, target_ids, submission_id) {
    mac_check_action_triggers(mac_ctx, action_type_id, target_ids, submission_id)
  }

  # actionTypesR::register_post_submit_hook() - if it exists
  if (exists("register_post_submit_hook", where = asNamespace("actionTypesR"))) {
    action_ctx <- actionTypesR::register_post_submit_hook(action_ctx, hook_fn)
  } else {
    # Fallback: attach hook directly to context
    if (is.null(action_ctx$post_submit_hooks)) action_ctx$post_submit_hooks <- list()
    action_ctx$post_submit_hooks <- c(action_ctx$post_submit_hooks, list(hook_fn))
    # Store the mac_ctx reference
    action_ctx$.mac_ctx <- mac_ctx
  }

  action_ctx
}

#' Check and apply action-triggered transitions
#'
#' @param mac_ctx A mac_context object.
#' @param action_type_id Character. The action type identifier that was submitted.
#' @param target_ids Character vector. Object keys that received the action.
#' @param submission_id Character or NULL. Optional submission reference.
#' @export
mac_check_action_triggers <- function(mac_ctx, action_type_id, target_ids, submission_id = NULL) {
  results <- list()

  if (length(target_ids) == 0) return(invisible(data.frame()))

  for (object_key in target_ids) {
    # Find all non-terminal instances for this object_key
    instances <- DBI::dbGetQuery(mac_ctx$con,
      "SELECT * FROM mac_instances WHERE object_key = ? AND completed_at IS NULL",
      params = list(as.character(object_key)))

    if (nrow(instances) == 0) next

    for (i in seq_len(nrow(instances))) {
      inst <- as.list(instances[i, ])
      proc <- tryCatch(mac_get_process(mac_ctx, inst$process_id), error = function(e) NULL)
      if (is.null(proc)) next

      action_trs <- Filter(
        function(tr) {
          tr$from == inst$current_state &&
          tr$trigger$type == "action" &&
          tr$trigger$action_type_id == action_type_id
        },
        proc$transitions
      )

      if (length(action_trs) == 0) next

      tr <- action_trs[[1]]

      # Check guard
      if (!is.null(tr$guard_fn)) {
        inst_df <- as.data.frame(instances[i, ])
        guarded <- tryCatch(
          isTRUE(tr$guard_fn(inst_df, list(ctx = mac_ctx))),
          error = function(e) FALSE
        )
        if (!guarded) next
      }

      tid <- mac_apply_transition(
        mac_ctx, inst$instance_id, tr$from, tr$to,
        trigger_type = "action",
        trigger_ref = action_type_id,
        submission_id = submission_id,
        process = proc
      )
      results[[length(results) + 1]] <- list(
        instance_id = inst$instance_id, from = tr$from, to = tr$to,
        transition_id = tid
      )
    }
  }

  if (length(results) == 0) return(invisible(data.frame()))
  dplyr::bind_rows(lapply(results, as.data.frame))
}
