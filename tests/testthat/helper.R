lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)

expect_fselect = function(fselect) {
  expect_r6(fselect, "FSelect",
    public = c("select", "param_set"),
    private = "select_internal"
  )
  expect_is(fselect$param_set, "ParamSet")
  expect_function(fselect$select, args = "instance")
}

expect_features = function(features, n) {
  res = max(rowSums(features))
  expect_equal(max(res), n)
}

make_dummy_feature_measure = function(type) {
  if (type == "classif") {
    id = "dummy.feature.classif"
    inh = MeasureClassif
    cl = "MeaureDummyCPClassif"
  } else {
    id = "dummy.feature.regr"
    inh = MeasureRegr
    cl = "MeaureDummyCPRegr"
  }
  m = R6Class(cl,
    inherit = inh,
    public = list(
      initialize = function() {
        super$initialize(
          id = id,
          range = c(0, 4),
          minimize = FALSE,
          properties = "requires_learner"
        )
      },

      score_internal = function(prediction, learner, task, ...) {
        if (test_names(task$feature_names, permutation.of = "Petal.Length")) {
          return(1)
        } else if (test_names(task$feature_names, permutation.of = c("Petal.Length", "Petal.Width"))) {
          return(2)
        } else if (test_names(task$feature_names, permutation.of = c("Petal.Length", "Petal.Width", "Sepal.Length"))) {
          return(4)
        } else if (test_names(task$feature_names, permutation.of = c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width"))) {
          return(3)
        } else {
          return(0)
        }
      }
    )
  )
}
MeasureDummyCPClassif = make_dummy_feature_measure("classif")
mlr3::mlr_measures$add("dummy.cp.classif", MeasureDummyCPClassif)
MeasureDummyCPRegr = make_dummy_feature_measure("regr")
mlr3::mlr_measures$add("dummy.cp.regr", MeasureDummyCPRegr)


TEST_MAKE_INST1 = function(values = NULL, folds = 2L, measures = msr("dummy.cp.classif"), term_evals = 5L) {
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
  inst = FSelectInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("dummy.cp.classif"), term, store_models = TRUE)
  fselect = fs(key, ...)
  expect_fselect(fselect)

  fselect$select(inst)
  data = inst$archive$data
  expect_data_table(data, nrows = real_evals)
  expect_equal(inst$archive$n_evals, real_evals)

  r = inst$result
  feat = r$feat
  perf = r$perf
  expect_character(feat)
  expect_numeric(perf)
  list(fselect = fselect, inst = inst)
}
