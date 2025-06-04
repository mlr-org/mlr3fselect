TEST_MAKE_TSK = function(n = 4L) {
  x = set_names(map_dtc(seq(n), function(x) rnorm(100L)), paste0("x", seq(n)))
  y = rnorm(100)
  TaskRegr$new(id = "mlr3fselect", backend = cbind(x, y), target = "y")
}

TEST_MAKE_INST_1D = function(n = 4L, folds = 2L, store_models = TRUE, store_benchmark_result = TRUE,
  measure = msr("dummy"), terminator = trm("evals", n_evals = 10)) {
  FSelectInstanceBatchSingleCrit$new(
    task = TEST_MAKE_TSK(n),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = folds),
    measure = measure,
    terminator = terminator,
    store_models = store_models,
    store_benchmark_result = store_benchmark_result)
}

TEST_MAKE_INST_2D = function(n = 4L, folds = 2L, store_models = FALSE, store_benchmark_result = TRUE) {
  FSelectInstanceBatchMultiCrit$new(
    task = TEST_MAKE_TSK(n),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = folds),
    measures = msrs(c("regr.mse", "regr.rmse")),
    terminator = trm("evals", n_evals = 10),
    store_models,
    store_benchmark_result = store_benchmark_result)
}

MeasureDummy = R6Class("MeasureDummy", inherit = MeasureRegr,
  public = list(
    initialize = function(score_design = NULL, minimize = FALSE) {
      if (is.null(score_design)) {
        score_design = data.table(
          score = c(1, 2, 4, 3),
          features = list("x1", c("x1", "x2"), c("x1", "x2", "x3"), c("x1", "x2", "x3", "x4"))
        )
      }
      private$.score_design = score_design
      super$initialize(id = "dummy", range = c(0, 4), minimize = minimize)
    }
  ),
    private = list(
      .score = function(prediction, learner, task, ...) {
        score = private$.score_design[sapply(get("features"), identical, task$feature_names), score]
        if (length(score) == 0) 0  else score
      },

      .score_design = NULL
    )
  )
mlr3::mlr_measures$add("dummy", MeasureDummy)

flush_redis = function() {
  config = redux::redis_config()
  r = redux::hiredis(config)
  r$FLUSHDB()
}

expect_rush_reset = function(rush, type = "kill") {
  rush$reset(type = type)
  Sys.sleep(1)
  keys = rush$connector$command(c("KEYS", "*"))
  if (!test_list(keys, len = 0)) {
    stopf("Found keys in redis after reset: %s", keys)
  }
}
