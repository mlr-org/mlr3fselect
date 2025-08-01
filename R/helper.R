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

extract_runtime = function(resample_result) {
  runtimes = map_dbl(get_private(resample_result)$.data$learner_states(), function(state) {
    state$train_time + state$predict_time
  })
  sum(runtimes)
}
