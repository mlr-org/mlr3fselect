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

test_that("fast aggregation works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  on.exit(mirai::daemons(0))
  mirai::daemons(1, seed = 123, dispatcher = FALSE)
  rush::rush_plan(n_workers = 1, worker_type = "remote")

  with_seed(123, {
    instance = fselect(
      fselector = fs("async_random_search"),
      task = task,
      learner = learner,
      resampling = resampling,
      measures = msr("classif.ce"),
      term_evals = 30
  )})

  expect_equal(instance$archive$data$classif.ce,
    instance$archive$benchmark_result$aggregate(msr("classif.ce"))$classif.ce)

  expect_equal(instance$archive$data$errors,
    instance$archive$benchmark_result$aggregate(msr("classif.ce"), conditions = TRUE)$errors)

  expect_equal(instance$archive$data$warnings,
    instance$archive$benchmark_result$aggregate(msr("classif.ce"), conditions = TRUE)$warnings)

  ce_fast = instance$archive$data$classif.ce

  mirai::daemons(0)
  on.exit(mirai::daemons(0))
  mirai::daemons(1, seed = 123, dispatcher = FALSE)
  rush::rush_plan(n_workers = 1, worker_type = "remote")

  with_seed(123, {
    instance = fselect(
      fselector = fs("async_random_search"),
      task = task,
      learner = learner,
      resampling = resampling,
      measures = msrs(c("classif.ce", "classif.acc")),
      term_evals = 30
  )})

  expect_equal(instance$archive$data$classif.ce,
    instance$archive$benchmark_result$aggregate(msr("classif.ce"))$classif.ce)

  expect_equal(instance$archive$data$errors,
    instance$archive$benchmark_result$aggregate(msr("classif.ce"), conditions = TRUE)$errors)

  expect_equal(instance$archive$data$warnings,
    instance$archive$benchmark_result$aggregate(msr("classif.ce"), conditions = TRUE)$warnings)


  ce_slow = instance$archive$data$classif.ce

  expect_equal(ce_fast, ce_slow)
})

test_that("fast aggregation conditions work", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  task = tsk("pima")
  learner = lrn("classif.debug", error_train = 0.1, warning_train = 0.1, error_predict = 0.1, warning_predict = 0.1)
  learner$encapsulate("evaluate", fallback = lrn("classif.featureless"))
  resampling = rsmp("cv", folds = 3)

  on.exit(mirai::daemons(0))
  mirai::daemons(1, seed = 123, dispatcher = FALSE)
  rush::rush_plan(n_workers = 1, worker_type = "remote")

  instance = fselect(
    fselector = fs("async_random_search"),
    task = task,
    learner = learner,
    resampling = resampling,
    measures = msr("classif.ce"),
    term_evals = 30
  )

  expect_equal(instance$archive$data$classif.ce,
    instance$archive$benchmark_result$aggregate(msr("classif.ce"))$classif.ce)

  expect_equal(instance$archive$data$errors,
    instance$archive$benchmark_result$aggregate(msr("classif.ce"), conditions = TRUE)$errors)

  expect_equal(instance$archive$data$warnings,
    instance$archive$benchmark_result$aggregate(msr("classif.ce"), conditions = TRUE)$warnings)
})
