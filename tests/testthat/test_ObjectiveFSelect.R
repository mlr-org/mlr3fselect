test_that("ObjectiveFSelect", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msr("dummy")

  obj = ObjectiveFSelect$new(task = task, learner = learner,
    resampling = resampling, measures = measures, store_models = TRUE)

  xss = list(
    list("x1" = TRUE, "x2" = FALSE, "x3" = TRUE, "x4" = TRUE),
    list("x1" = FALSE, "x2" = TRUE, "x3" = TRUE, "x4" = TRUE))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 2)
  expect_equal(obj$archive$benchmark_result$resample_result(1)$learners[[1]]$model$select$selection, c("x1", "x3", "x4"))
  expect_equal(obj$archive$benchmark_result$resample_result(2)$learners[[1]]$model$select$selection, c("x2", "x3", "x4"))
})

test_that("ObjectiveFSelect works with multiple measures", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msrs(c("regr.mse", "regr.rmse"))

  obj = ObjectiveFSelect$new(task = task, learner = learner,
    resampling = resampling, measures = measures, store_models = TRUE)

  xss = list(
    list("x1" = TRUE, "x2" = FALSE, "x3" = TRUE, "x4" = TRUE),
    list("x1" = FALSE, "x2" = TRUE, "x3" = TRUE, "x4" = TRUE))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 3)
})

test_that("ObjectiveFSelect works with store_models", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msr("dummy")

  obj = ObjectiveFSelect$new(task = task, learner = learner,
    resampling = resampling, measures = measures,
    store_models = TRUE)

  xss = list(
    list("x1" = TRUE, "x2" = FALSE, "x3" = TRUE, "x4" = TRUE),
    list("x1" = FALSE, "x2" = TRUE, "x3" = TRUE, "x4" = TRUE))

  z = obj$eval_many(xss)
  expect_class(
    obj$archive$benchmark_result$resample_result(1)$learners[[1]]$model$regr.rpart$model,
    "rpart")
})
