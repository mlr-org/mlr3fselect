task_to_domain = function(task) {
  params = rep(list(p_lgl()), length(task$feature_names))
  names(params) = task$feature_names
  do.call(ps, params)
}

measures_to_codomain = function(measures) {
  measures = as_measures(measures)
  domains = map(measures, function(s) {
    if ("set_id" %in% names(ps())) {
      # old paradox
      get("ParamDbl")$new(id = s$id, tags = ifelse(s$minimize, "minimize", "maximize"))
    } else {
      p_dbl(tags = ifelse(s$minimize, "minimize", "maximize"))
    }
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

# Sums the train and predict times of all learners of a resample result.
# PRIVATE MLR3 API: `$.data$learner_states()` and `$.view` are private fields of `mlr3::ResampleResult`.
# There is no public accessor for the learner states, and `$score()` would compute a measure we do not need.
# The layout is stable for the mlr3 versions supported in DESCRIPTION (>= 1.0.1) and must be re-checked on every
# mlr3 update. See https://github.com/mlr-org/mlr3/issues for an upstream request for public accessors.
extract_runtime = function(resample_result) {
  runtimes = map_dbl(
    get_private(resample_result)$.data$learner_states(get_private(resample_result)$.view),
    function(state) {
      state$train_time + state$predict_time
    }
  )
  sum(runtimes)
}
