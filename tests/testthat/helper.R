TEST_MAKE_TSK = function(n = 4L) {
  x = set_names(map_dtc(seq(n), function(x) rnorm(100L)),
    paste0("x", seq(n)))
  y = rnorm(100)
  TaskRegr$new(id = "mlr3fselect", backend = cbind(x, y), target = "y")
}

expect_fselector = function(fselector) {
  expect_r6(fselector, "FSelector",
    public = c("optimize", "param_set", "properties", "packages"),
    private = c(".optimize", ".assign_result"))
}

expect_feature_number = function(features, n) {
  res = rowSums(features)
  expect_set_equal(res, n)
}

expect_max_features = function(features, n) {
  res = max(rowSums(features))
  expect_set_equal(res, n)
}

MeasureDummy =
  R6Class("MeasureDummy", inherit = MeasureRegr,
    public = list(
      initialize = function() {
        super$initialize(id = "dummy", range = c(0, 4), minimize = FALSE)
      }
    ),
    private = list(

      .score = function(prediction, learner, task, ...) {
        if (test_names(task$feature_names, permutation.of = "x1")) {
          return(1)
        } else if (test_names(task$feature_names,
          permutation.of = c("x1", "x2"))) {
          return(2)
        } else if (test_names(task$feature_names,
          permutation.of = c("x1", "x2", "x3"))) {
          return(4)
        } else if (test_names(task$feature_names,
          permutation.of = c("x1", "x2", "x3", "x4"))) {
          return(3)
        } else {
          return(0)
        }
      }
    )
  )
mlr3::mlr_measures$add("dummy", MeasureDummy)

test_fselector = function(.key, ..., term_evals = 2L, real_evals = term_evals,
  store_models = FALSE) {

  inst = FSelectInstanceSingleCrit$new(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measure = msr("dummy"),
    terminator = trm("evals", n_evals = term_evals),
    store_models)

  fselector = fs(.key, ...)
  expect_fselector(fselector)
  expect_data_table(fselector$optimize(inst), nrows = 1, ncols = 7,
    any.missing = FALSE)
  archive = inst$archive

  # Archive checks
  expect_data_table(archive$data(), nrows = real_evals)
  expect_equal(inst$archive$n_evals, real_evals)

  # Result checks
  expect_data_table(inst$result, nrows = 1, ncols = 7)
  expect_named(inst$result, c(
    "x1",
    "x2",
    "x3",
    "x4",
    "features",
    "x_domain",
    "dummy"))
  expect_character(inst$result$features[[1]])
  expect_named(inst$result_x_domain, c(
    "x1",
    "x2",
    "x3",
    "x4"))
  expect_data_table(inst$result_x_search_space, nrows = 1, ncols = 4,
    types = "logical")
  expect_named(inst$result_x_search_space, c(
    "x1",
    "x2",
    "x3",
    "x4"))
  expect_named(inst$result_y, "dummy")

  list(fselector = fselector, inst = inst)
}

TEST_MAKE_INST_1D = function(n = 4L, folds = 2L, store_models = FALSE) {
  FSelectInstanceSingleCrit$new(
    task = TEST_MAKE_TSK(n),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = folds),
    measure = msr("dummy"),
    terminator = trm("evals", n_evals = 10),
    store_models)
}

TEST_MAKE_INST_2D = function(n = 4L, folds = 2L, store_models = FALSE) {
  FSelectInstanceMultiCrit$new(
    task = TEST_MAKE_TSK(n),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = folds),
    measure = msrs(c("regr.mse", "regr.rmse")),
    terminator = trm("evals", n_evals = 10),
    store_models)
}
