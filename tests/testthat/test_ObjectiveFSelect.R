context("ObjectiveFSelect")

test_that("ObjectiveFSelect", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measures = msr("dummy.cp.classif")

  obj = ObjectiveFSelect$new(task = task, learner = learner,
    resampling = resampling, measures = measures)

  xss = list(
    list("Petal.Length" = TRUE,
      "Petal.Width" = FALSE,
      "Sepal.Length" = TRUE,
      "Sepal.Width" = TRUE),
    list("Petal.Length" = FALSE,
      "Petal.Width" = TRUE,
      "Sepal.Length" = TRUE,
      "Sepal.Width" = TRUE))

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 2)
  expect_equal(z$resample_result[[1]]$task$feature_names, c("Petal.Length",
    "Sepal.Length", "Sepal.Width"))
  expect_equal(z$resample_result[[2]]$task$feature_names, c("Petal.Width",
    "Sepal.Length", "Sepal.Width"))
  expect_null(z$resample_result[[1]]$learners[[1]]$model)

  measures = msrs(c("dummy.cp.classif", "classif.ce"))
  obj = ObjectiveFSelect$new(task = task, learner = learner,
    resampling = resampling, measures = measures)

  z = obj$eval_many(xss)
  expect_data_table(z, nrows = 2, ncols = 3)

  obj = ObjectiveFSelect$new(task = task, learner = learner,
                             resampling = resampling, measures = measures,
                             store_models = TRUE)

  z = obj$eval_many(xss)
  expect_class(z$resample_result[[1]]$learners[[1]]$model, "rpart")
})
