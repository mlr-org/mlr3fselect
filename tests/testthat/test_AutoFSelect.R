context("AutoFSelector")

test_that("train and predict work", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msr("dummy")
  fselector = fs("sequential")
  terminator = trm("evals", n_evals = 4L)

  at = AutoFSelector$new(learner, resampling, measures, terminator, fselector)
  expect_learner(at)
  at$train(task)
  expect_learner(at)
  inst = at$fselect_instance
  a = inst$archive$data()
  expect_data_table(a, nrows = 4L)
  r = at$fselect_result
  expect_equal(r$x1, TRUE)
  expect_equal(r$x2, FALSE)
  expect_equal(r$x3, FALSE)
  expect_equal(r$x4, FALSE)
  prd = at$predict(task)
  expect_prediction(prd)
  expect_is(at$learner$model, "rpart")
})

test_that("nested resampling works", {
  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling_inner = rsmp("holdout")
  measures = msr("dummy")
  fselector = fs("sequential")
  terminator = trm("evals", n_evals = 10L)

  at = AutoFSelector$new(learner, resampling_inner, measures, terminator,
    fselector = fselector)
  expect_null(at$fselect_instance)

  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = TRUE)
  tab = as.data.table(rr)

  expect_data_table(tab, nrows = 2)
  expect_data_table(tab$learner[[1]]$archive$data(), nrows = 10)
  expect_data_table(tab$learner[[2]]$archive$data(), nrows = 10)
  expect_equal(tab$learner[[1]]$fselect_result$x1, TRUE)
  expect_equal(tab$learner[[1]]$fselect_result$x2, TRUE)
  expect_equal(tab$learner[[1]]$fselect_result$x3, TRUE)
  expect_equal(tab$learner[[1]]$fselect_result$x4, FALSE)
  expect_equal(tab$learner[[2]]$fselect_result$x1, TRUE)
  expect_equal(tab$learner[[2]]$fselect_result$x2, TRUE)
  expect_equal(tab$learner[[2]]$fselect_result$x3, TRUE)
  expect_equal(tab$learner[[2]]$fselect_result$x4, FALSE)
  expect_numeric(tab$learner[[1]]$fselect_result$dummy, len = 1L)
  expect_numeric(tab$learner[[2]]$fselect_result$dummy, len = 1L)
  expect_class(tab$learner[[1]]$model$learner$model, "rpart")
  expect_class(tab$learner[[2]]$model$learner$model, "rpart")
  expect_null(at$archive)
})

test_that("store_fselect_instance, store_benchmark_result and store_models flags work", {
  te = trm("evals", n_evals = 10)
  task = tsk("iris")
  ms = msr("classif.ce")
  fselector = fs("random_search")

  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = TRUE, store_benchmark_result = TRUE,
    store_models = TRUE)
  at$train(task)

  assert_r6(at$fselect_instance, "FSelectInstanceSingleCrit")
  assert_benchmark_result(at$fselect_instance$archive$benchmark_result)
  assert_class(at$fselect_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model, "rpart")

  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = TRUE, store_benchmark_result = TRUE,
    store_models = FALSE)
  at$train(task)

  assert_r6(at$fselect_instance, "FSelectInstanceSingleCrit")
  assert_benchmark_result(at$fselect_instance$archive$benchmark_result)
  assert_null(at$fselect_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model)

  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = TRUE, store_benchmark_result = FALSE,
    store_models = FALSE)
  at$train(task)

  assert_r6(at$fselect_instance, "FSelectInstanceSingleCrit")
  assert_null(at$fselect_instance$archive$benchmark_result)

  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = FALSE, store_benchmark_result = FALSE,
    store_models = FALSE)
  at$train(task)

  assert_null(at$fselect_instance)

  expect_error(AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = FALSE, store_benchmark_result = TRUE,
    store_models = FALSE),
  regexp = "Benchmark results can only be stored if store_fselect_instance is set to TRUE",
  fixed = TRUE)

  expect_error(AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = TRUE, store_benchmark_result = FALSE,
    store_models = TRUE),
  regexp = "Models can only be stored if store_benchmark_result is set to TRUE",
  fixed = TRUE)

  expect_error(AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = FALSE, store_benchmark_result = FALSE,
    store_models = TRUE),
  regexp = "Models can only be stored if store_benchmark_result is set to TRUE",
  fixed = TRUE)
})
