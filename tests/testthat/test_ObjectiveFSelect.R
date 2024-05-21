test_that("ObjectiveFSelectBatch", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msr("dummy")

  archive = ArchiveBatchFSelect$new(search_space = task_to_domain(task), codomain = measures_to_codomain(measures))

  obj = ObjectiveFSelectBatch$new(task = task, learner = learner, resampling = resampling, measures = measures, archive = archive)

  xss = list(
    list("x1" = TRUE, "x2" = FALSE, "x3" = TRUE, "x4" = TRUE),
    list("x1" = FALSE, "x2" = TRUE, "x3" = TRUE, "x4" = TRUE))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 5)
  expect_equal(obj$archive$benchmark_result$resample_result(1)$task$feature_names, c("x1", "x3", "x4"))
  expect_equal(obj$archive$benchmark_result$resample_result(2)$task$feature_names, c("x2", "x3", "x4"))
  expect_null(obj$archive$benchmark_result$resample_result(1)$learners[[1]]$model)
})

test_that("ObjectiveFSelectBatch works with multiple measures", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msrs(c("regr.mse", "regr.rmse"))

  archive = ArchiveBatchFSelect$new(search_space = task_to_domain(task), codomain = measures_to_codomain(measures))
  obj = ObjectiveFSelectBatch$new(task = task, learner = learner, resampling = resampling, measures = measures, archive = archive)

  xss = list(
    list("x1" = TRUE, "x2" = FALSE, "x3" = TRUE, "x4" = TRUE),
    list("x1" = FALSE, "x2" = TRUE, "x3" = TRUE, "x4" = TRUE))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 6)
})

test_that("ObjectiveFSelectBatch works with store_models", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msr("dummy")

   archive = ArchiveBatchFSelect$new(search_space = task_to_domain(task), codomain = measures_to_codomain(measures))
  obj = ObjectiveFSelectBatch$new(task = task, learner = learner,
    resampling = resampling, measures = measures, archive = archive,
    store_models = TRUE)

  xss = list(
    list("x1" = TRUE, "x2" = FALSE, "x3" = TRUE, "x4" = TRUE),
    list("x1" = FALSE, "x2" = TRUE, "x3" = TRUE, "x4" = TRUE))

  z = obj$eval_many(xss)
  expect_class(obj$archive$benchmark_result$resample_result(1)$learners[[1]]$model, "rpart")
})
