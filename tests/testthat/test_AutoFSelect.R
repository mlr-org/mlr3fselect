context("AutoFSelect")

test_that("AutoFSelect - train and predict", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msrs(c("dummy.sequential", "regr.mse"))
  fs = fs("sequential")
  terminator = term("evals", n_evals = 4L)

  at = AutoFSelect$new(learner, resampling, measures, terminator, fselect = fs)
  expect_learner(at)
  at$train(task)
  expect_learner(at)
  inst = at$fselect_instance
  a = inst$archive$data
  expect_data_table(a, nrows = 4L)
  r = at$fselect_result
  expect_equal(r$feat, c("x1"))
  prd = at$predict(task)
  expect_prediction(prd)
  expect_is(at$learner$model, "rpart")
})

test_that("AutoFSelect - resample", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling_inner = rsmp("holdout")
  measures = msr("dummy.sequential")
  fs = fs("sequential")
  terminator = term("evals", n_evals = 10L)

  at = AutoFSelect$new(learner, resampling_inner, measures, terminator, fselect = fs)
  expect_null(at$fselect_instance)

  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = TRUE)

  expect_data_table(rr$data, nrows = 2)
  expect_data_table(rr$data$learner[[1]]$archive$data, nrows = 10)
  expect_data_table(rr$data$learner[[2]]$archive$data, nrows = 10)
  expect_equal(rr$data$learner[[1]]$fselect_result$feat, c("x1", "x2", "x3"))
  expect_equal(rr$data$learner[[2]]$fselect_result$feat, c("x1", "x2", "x3"))
  expect_numeric(rr$data$learner[[1]]$fselect_result$perf, len = 1L)
  expect_numeric(rr$data$learner[[2]]$fselect_result$perf, len = 1L)
  expect_class(rr$data$learner[[1]]$model$learner$model, "rpart")
  expect_class(rr$data$learner[[2]]$model$learner$model, "rpart")
  expect_null(at$archive)
})
