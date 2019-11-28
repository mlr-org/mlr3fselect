lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)

expect_fselect = function(fselect) {
  expect_r6(fselect, "FSelect",
            public = c("select", "param_set"),
            private = "select_internal"
  )
  expect_is(fselect$param_set, "ParamSet")
  expect_function(fselect$select, args = "instance")
}


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

test_fselect = function(key, ..., term_evals = 2L, real_evals = term_evals) {
  term = term("evals", n_evals = term_evals)
  inst = FSelectInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term)
  fselect = fs(key, ...)
  expect_fselect(fselect)

  fselect$select(inst)
  bmr = inst$bmr
  expect_data_table(bmr$data, nrows = real_evals)
  expect_equal(inst$n_evals, real_evals)
}
