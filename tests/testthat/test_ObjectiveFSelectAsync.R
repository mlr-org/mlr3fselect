test_that("objective async works", {
  task = TEST_MAKE_TSK()

  objective = ObjectiveFSelectAsync$new(
    task = task,
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("dummy"),
    store_models = FALSE,
    store_benchmark_result = FALSE
  )

  # Create a feature subset (select first 3 features)
  xs = list("x1" = TRUE, "x2" = FALSE, "x3" = TRUE, "x4" = TRUE)
  y = objective$eval(xs)

  expect_names(names(y), permutation.of = c("dummy", "runtime_learners", "warnings", "errors"))
  expect_number(y$dummy)
  expect_number(y$runtime_learners)
  expect_number(y$warnings)
  expect_number(y$errors)
})

test_that("store benchmark result works", {
  task = TEST_MAKE_TSK()
  objective = ObjectiveFSelectAsync$new(
    task = task,
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("dummy"),
    store_models = FALSE,
    store_benchmark_result = TRUE
  )

  # Create a feature subset (select first 3 features)
  xs = list("x1" = TRUE, "x2" = FALSE, "x3" = TRUE, "x4" = TRUE)
  y = objective$eval(xs)

  expect_names(names(y), permutation.of = c("dummy", "runtime_learners", "resample_result", "warnings", "errors"))
  expect_number(y$dummy)
  expect_number(y$runtime_learners)
  expect_resample_result(y$resample_result[[1]])
  expect_null(y$resample_result[[1]]$learners[[1]]$model)
  expect_number(y$warnings)
  expect_number(y$errors)
})

test_that("store models works", {
  task = TEST_MAKE_TSK()
  objective = ObjectiveFSelectAsync$new(
    task = task,
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("dummy"),
    store_models = TRUE,
    store_benchmark_result = TRUE
  )

  # Create a feature subset (select first 3 features)
  xs = list("x1" = TRUE, "x2" = FALSE, "x3" = TRUE, "x4" = TRUE)
  y = objective$eval(xs)

  expect_names(names(y), permutation.of = c("dummy", "runtime_learners", "resample_result", "warnings", "errors"))
  expect_number(y$dummy)
  expect_number(y$runtime_learners)
  expect_resample_result(y$resample_result[[1]])
  expect_class(y$resample_result[[1]]$learners[[1]]$model, "rpart")
  expect_number(y$warnings)
  expect_number(y$errors)
})

test_that("rush objective with multiple measures works", {
  task = TEST_MAKE_TSK()
  objective = ObjectiveFSelectAsync$new(
    task = task,
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = c(msr("dummy", id = "dummy1"), msr("dummy", id = "dummy2")),
    store_models = FALSE,
    store_benchmark_result = FALSE
  )

  # Create a feature subset (select first 3 features)
  xs = list("x1" = TRUE, "x2" = FALSE, "x3" = TRUE, "x4" = TRUE)
  y = objective$eval(xs)

  expect_names(names(y), permutation.of = c("dummy1", "dummy2", "runtime_learners", "warnings", "errors"))
  expect_number(y$dummy1)
  expect_number(y$dummy2)
  expect_number(y$runtime_learners)
  expect_number(y$warnings)
  expect_number(y$errors)
})
