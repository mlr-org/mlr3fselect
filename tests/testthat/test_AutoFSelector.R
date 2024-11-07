test_that("train and predict work", {
  skip_on_cran()

  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")
  measures = msr("dummy")
  fselector = fs("sequential")
  terminator = trm("evals", n_evals = 4L)

  at = AutoFSelector$new(fselector, learner, resampling, measures, terminator)
  expect_learner(at)
  at$train(task)
  expect_learner(at)
  inst = at$fselect_instance
  a = inst$archive$data
  expect_data_table(a, nrows = 4L)
  r = at$fselect_result
  expect_equal(r$x1, TRUE)
  expect_equal(r$x2, FALSE)
  expect_equal(r$x3, FALSE)
  expect_equal(r$x4, FALSE)
  prd = at$predict(task)
  expect_prediction(prd)
  expect_s3_class(at$learner$model, "rpart")
})

test_that("nested resampling works", {
  skip_on_cran()

  task = TEST_MAKE_TSK()
  learner = lrn("regr.rpart")
  resampling_inner = rsmp("holdout")
  measures = msr("dummy")
  fselector = fs("sequential")
  terminator = trm("evals", n_evals = 10L)

  at = AutoFSelector$new(fselector, learner, resampling_inner, measures, terminator, store_models = TRUE)
  expect_null(at$fselect_instance)

  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = TRUE)
  tab = as.data.table(rr)

  expect_data_table(tab, nrows = 2)
  expect_data_table(tab$learner[[1]]$archive$data, nrows = 10)
  expect_data_table(tab$learner[[2]]$archive$data, nrows = 10)
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
  skip_on_cran()

  te = trm("evals", n_evals = 2)
  task = tsk("iris")
  ms = msr("classif.ce")
  fselector = fs("random_search")

  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = TRUE, store_benchmark_result = TRUE,
    store_models = TRUE)
  at$train(task)

  expect_r6(at$fselect_instance, "FSelectInstanceBatchSingleCrit")
  expect_benchmark_result(at$fselect_instance$archive$benchmark_result)
  expect_class(at$fselect_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model, "rpart")

  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = TRUE, store_benchmark_result = TRUE,
    store_models = FALSE)
  at$train(task)

  expect_r6(at$fselect_instance, "FSelectInstanceBatchSingleCrit")
  expect_benchmark_result(at$fselect_instance$archive$benchmark_result)
  expect_null(at$fselect_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model)

  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = TRUE, store_benchmark_result = FALSE,
    store_models = FALSE)
  at$train(task)

  expect_r6(at$fselect_instance, "FSelectInstanceBatchSingleCrit")
  expect_equal(at$fselect_instance$archive$benchmark_result$n_resample_results, 0L)

  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = FALSE, store_benchmark_result = FALSE,
    store_models = FALSE)
  at$train(task)

  expect_null(at$fselect_instance)

  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = FALSE, store_benchmark_result = FALSE,
    store_models = TRUE)
  at$train(task)

  expect_r6(at$fselect_instance, "FSelectInstanceBatchSingleCrit")
  expect_benchmark_result(at$fselect_instance$archive$benchmark_result)
  expect_class(at$fselect_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model, "rpart")

  at = AutoFSelector$new(lrn("classif.rpart"), rsmp("holdout"), ms, te,
    fselector = fselector, store_fselect_instance = FALSE, store_benchmark_result = TRUE,
    store_models = FALSE)
  at$train(task)

  expect_r6(at$fselect_instance, "FSelectInstanceBatchSingleCrit")
  expect_benchmark_result(at$fselect_instance$archive$benchmark_result)
  expect_null(at$fselect_instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model)
})

test_that("AutoFSelector works with GraphLearner", {
  skip_if_not_installed("mlr3pipelines")
  library("mlr3pipelines")

  task = TEST_MAKE_TSK()
  graph =  po("imputemedian") %>>% lrn("regr.rpart")
  learner = GraphLearner$new(graph)
  resampling = rsmp("holdout")
  measures = msr("dummy")
  fselector = fs("sequential")
  terminator = trm("evals", n_evals = 4L)

  at = AutoFSelector$new(fselector, learner, resampling, measures, terminator)
  expect_learner(at)
  at$train(task)
  expect_learner(at)
  inst = at$fselect_instance
  a = inst$archive$data
  expect_data_table(a, nrows = 4L)
  r = at$fselect_result
  expect_equal(r$x1, TRUE)
  expect_equal(r$x2, FALSE)
  expect_equal(r$x3, FALSE)
  expect_equal(r$x4, FALSE)
  prd = at$predict(task)
  expect_prediction(prd)
  expect_equal(at$model$features, "x1")
  # Check for clone
  expect_equal(task$feature_names, c("x1", "x2", "x3" ,"x4"))
})

test_that("AutoFSelector get_base_learner method works", {
  skip_if_not_installed("mlr3pipelines")
  library("mlr3pipelines")

  # simple learner
  learner = lrn("classif.rpart")
  afs = AutoFSelector$new(
    fselector = fs("random_search"),
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1))
  afs$train(tsk("pima"))

  expect_learner(afs$base_learner())
  expect_equal(afs$base_learner()$id, "classif.rpart")
  expect_learner(afs$base_learner(recursive = 0))
  expect_equal(afs$base_learner(recursive = 0)$id, "classif.rpart")

  # graph learner
  learner = as_learner(pipeline_robustify() %>>% lrn("classif.rpart"))
  learner$id = "graphlearner.classif.rpart"
  afs = AutoFSelector$new(
    fselector = fs("random_search"),
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1))
  afs$train(tsk("pima"))

  expect_learner(afs$base_learner(recursive = 0))
  expect_equal(afs$base_learner(recursive = 0)$id, "graphlearner.classif.rpart")
  # expect_learner(afs$base_learner())
  # expect_equal(afs$base_learner()$id, "classif.rpart")
})

test_that("AutoFSelector hash works #647 in mlr3", {
  afs_1 = AutoFSelector$new(
    id = "afs_1",
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    fselector = fs("random_search"),
    store_benchmark_result = FALSE)

  afs_2 = AutoFSelector$new(
    id = "afs_2",
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    fselector = fs("random_search"),
    store_benchmark_result = TRUE)

  resampling_outer = rsmp("holdout")
  grid = benchmark_grid(tsk("iris"), list(afs_1, afs_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  expect_data_table(bmr$learners, nrows = 2)
})
