# Run recursive feature elimination
# instance is changed by reference
rfe_workhorse = function(inst, subsets, recursive, aggregation = raw_importance, fold = NULL) {
  archive = inst$archive
  feature_names = inst$archive$cols_x
  n = length(feature_names)

  # Create full feature set
  states = set_names(as.list(rep(TRUE, n)), feature_names)
  states = as.data.table(states)
  inst$eval_batch(states)

  # Calculate the variable importance on the full feature set
  uhash = archive$data[get("batch_nr") == archive$n_evals, uhash]
  rr = archive$benchmark_result$resample_result(uhash = uhash)
  imp = aggregation(rr$learners, feature_names)

  # Log importance and fold to archive
  set(archive$data, archive$n_evals, "importance", list(imp))
  if (!is.null(fold)) set(archive$data, archive$n_evals, "fold", fold)

  for (j in subsets) {
    if (recursive) {

      # Eliminate the most unimportant features
      feat = archive$data[get("batch_nr") == archive$n_batch, archive$cols_x, with = FALSE]
      feat = feature_names[as.logical(feat)]
      states = set_names(as.list(feature_names %in% feat & feature_names %in% names(imp[seq(j)])), feature_names)
      states = as.data.table(states)

      # Fit model on the reduced feature subset
      inst$eval_batch(states)

      # Recalculate the variable importance on the reduced feature subset
      uhash = archive$data[get("batch_nr") == archive$n_batch, uhash]
      rr = archive$benchmark_result$resample_result(uhash = uhash)
      feat = feature_names[as.logical(states)]
      imp = aggregation(rr$learners, feat)

      # Log importance to archive
      set(archive$data, archive$n_evals, "importance", list(imp))
    } else {

      # Eliminate the most unimportant features
      states = set_names(as.list(feature_names %in% names(imp[seq(j)])), feature_names)
      states = as.data.table(states)

      # Fit model on the reduced feature subset
      inst$eval_batch(states)

      # Log importance to archive
      set(archive$data, archive$n_evals, "importance", list(imp[seq(j)]))
    }
    if (!is.null(fold)) set(archive$data, archive$n_evals, "fold", fold)
  }
}
