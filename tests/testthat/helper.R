lapply(list.files(system.file("testthat", package = "mlr3"),
  pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)

# Returns regression task containing n features
TEST_MAKE_TSK = function(n_features = 4L) {
  x = set_names(map_dtc(seq(n_features), function(x) rnorm(100L)),
    paste0("x", seq(n_features)))
  y = rnorm(100)
  TaskRegr$new(id = "mlr3fselect", backend = cbind(x, y), target = "y")
}

# Returns feature parameter set containing n features
TEST_MAKE_PS = function(n_features = 4L) {
  ParamSet$new(map(paste0("x", seq(n_features)),
    function(x) ParamLgl$new(id = x)))
}

# Returns FSelectInstance
TEST_MAKE_INST = function(n_features = 4L, folds = 2L,
  measures = msr("dummy.sequential"), term_evals = 5L, store_models = FALSE) {
  learner = lrn("regr.rpart")
  task = TEST_MAKE_TSK(n_features)
  resampling = rsmp("cv", folds = folds)
  terminator = trm("evals", n_evals = term_evals)

  inst = FSelectInstanceSingleCrit$new(task, learner, resampling, measures, terminator,
    store_models)
  return(inst)
}

# Dummy measure for sequentially operating aligorithms
MeasureDummySequential =
  R6Class("MeasureDummySequential", inherit = MeasureRegr,
    public = list(
      initialize = function() {
        super$initialize(
          id = "dummy.sequential",
          range = c(0, 4),
          minimize = FALSE,
          properties = "requires_learner"
        )
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
mlr3::mlr_measures$add("dummy.sequential", MeasureDummySequential)

# Test an implemented subclass fselect by running a couple of standard tests
test_fselect = function(key, ..., term_evals = 2L, real_evals = term_evals,
  store_models = FALSE) {

  inst = TEST_MAKE_INST(term_evals = term_evals, store_models = store_models)
  fselect = fs(key, ...)
  expect_fselect(fselect)

  fselect$optimize(inst)
  data = inst$archive$data()
  expect_data_table(data, nrows = real_evals)
  expect_equal(inst$archive$n_evals, real_evals)

  r = inst$result
  #feat = r$feat
  #perf = r$perf
  #expect_character(feat)
  #expect_numeric(perf)
  list(fselect = fselect, inst = inst)
}

# Check FSelect subclass
expect_fselect = function(fselect) {
  expect_r6(fselect, "FSelector",
    public = c("optimize", "param_set"),
    private = ".optimize"
  )
  expect_is(fselect$param_set, "ParamSet")
  expect_function(fselect$optimize, args = "inst")
}

# Check expected feature number
expect_features = function(features, n) {
  res = max(rowSums(features))
  expect_equal(max(res), n)
}
