TEST_MAKE_PS1 = function(n_dim = 1L) {
  if (n_dim == 1) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3)
    ))
  } else if (n_dim == 2) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3),
      ParamInt$new("minsplit", lower = 1, upper = 9)
    ))
  }
}

TEST_MAKE_INST1 = function(values = NULL, folds = 2L, measures = msr("classif.ce"), n_dim = 1L, term_evals = 5L) {
  ps = TEST_MAKE_PS1(n_dim = n_dim)
  lrn = mlr_learners$get("classif.rpart")
  if (!is.null(values)) {
    lrn$param_set$values = values
  }
  rs = rsmp("cv", folds = folds)
  term = term("evals", n_evals = term_evals)
  inst = FSelectInstance$new(tsk("iris"), lrn, rs, measures, term)
  return(inst)
}
