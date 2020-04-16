context("ObjectiveFSelect")

test_that("ObjectiveFSelect", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msr("dummy.sequential")

  obj = ObjectiveFSelect$new(task = task, learner = learner,
    resampling = resampling, measures = measures)

  xss = list(
    list("x1" = TRUE,  "x2" = FALSE, "x3" = TRUE, "x4" = TRUE),
    list("x1" = FALSE,  "x2" = TRUE, "x3" = TRUE, "x4" = TRUE))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 2)
  expect_equal(z$resample_result[[1]]$task$feature_names, c("x1", "x3", "x4"))
  expect_equal(z$resample_result[[2]]$task$feature_names, c("x2", "x3", "x4"))
  expect_null(z$resample_result[[1]]$learners[[1]]$model)
})

test_that("ObjectiveFSelect - Multiple measures", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msrs(c("dummy.sequential", "regr.mse"))

  obj = ObjectiveFSelect$new(task = task, learner = learner,
                             resampling = resampling, measures = measures)

  xss = list(
    list("x1" = TRUE,  "x2" = FALSE, "x3" = TRUE, "x4" = TRUE),
    list("x1" = FALSE,  "x2" = TRUE, "x3" = TRUE, "x4" = TRUE))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 3)
})

test_that("ObjectiveFSelect - Store models", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msr("dummy.sequential")

  obj = ObjectiveFSelect$new(task = task, learner = learner,
                             resampling = resampling, measures = measures,
                             store_models = TRUE)

  xss = list(
    list("x1" = TRUE,  "x2" = FALSE, "x3" = TRUE, "x4" = TRUE),
    list("x1" = FALSE,  "x2" = TRUE, "x3" = TRUE, "x4" = TRUE))

  z = obj$eval_many(xss)
  expect_class(z$resample_result[[1]]$learners[[1]]$model, "rpart")
})


