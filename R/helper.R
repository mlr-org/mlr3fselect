task_to_domain = function(task) {
  params = rep(list(p_lgl()), length(task$feature_names))
  names(params) = task$feature_names
  do.call(ps, params)
}

measures_to_codomain = function(measures) {
  measures = as_measures(measures)
  domains = map(measures, function(s) {
    p_dbl(tags = if (s$minimize) "minimize" else "maximize")
  })
  names(domains) = ids(measures)
  Codomain$new(domains)
}

# Restricts the task to the selected features and the features with the `always_included` column role.
# The always included columns must be converted to features because a learner is only trained on the columns with the
# `feature` column role.
# The task is changed by reference.
select_features = function(task, features) {
  always_included = task$col_roles$always_included
  task$set_col_roles(always_included, "feature")
  task$select(c(features, always_included))
  task
}

extract_runtime = function(resample_result) {
  runtimes = map_dbl(
    get_private(resample_result)$.data$learner_states(get_private(resample_result)$.view),
    function(state) {
      state$train_time + state$predict_time
    }
  )
  sum(runtimes)
}
