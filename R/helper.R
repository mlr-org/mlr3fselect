task_to_domain = function(task) {
  ParamSet$new(map(task$feature_names, function(s) ParamLgl$new(id = s)))
}

measures_to_codomain = function(measures) {
  Codomain$new(map(as_measures(measures), function(s) {
    ParamDbl$new(id = s$id, tags = ifelse(s$minimize, "minimize", "maximize"))
  }))
}
