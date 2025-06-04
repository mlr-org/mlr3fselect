test_that("initializing FSelectInstanceAsyncSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3)
  )

  expect_r6(instance$archive, "ArchiveAsyncFSelect")
  expect_r6(instance$objective, "Objective")
  expect_r6(instance$search_space, "ParamSet")
  expect_r6(instance$terminator, "Terminator")
  expect_r6(instance$rush, "Rush")
  expect_null(instance$result)

  expect_rush_reset(instance$rush, type = "kill")
})

test_that("rush controller can be passed to FSelectInstanceAsyncSingleCrit", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush = rush::rsh(network_id = "remote_network")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  expect_class(instance$rush, "Rush")
  expect_equal(instance$rush$network_id, "remote_network")
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("FSelectInstanceAsyncSingleCrit can be passed to a fselector", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3)
  )

  fselector = fs("async_random_search")
  fselector$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 3L)
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("assigning a result to FSelectInstanceAsyncSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3)
  )

  fselector = fs("async_random_search")
  fselector$optimize(instance)

  result = instance$result
  expect_data_table(result, nrows = 1)
  expect_names(names(result), must.include = c("features", "n_features", "classif.ce"))
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("saving the benchmark result with FSelectInstanceAsyncSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE
  )

  fselector = fs("async_random_search")
  fselector$optimize(instance)

  expect_benchmark_result(instance$archive$benchmark_result)
  expect_gte(instance$archive$benchmark_result$n_resample_results, 3L)
  expect_null(instance$archive$resample_result(1)$learners[[1]]$model)
  expect_rush_reset(instance$rush, type = "kill")
})

test_that("saving the models with FSelectInstanceAsyncSingleCrit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    store_models = TRUE
  )

  fselector = fs("async_random_search")
  fselector$optimize(instance)

  expect_benchmark_result(instance$archive$benchmark_result)
  expect_gte(instance$archive$benchmark_result$n_resample_results, 3L)
  expect_class(instance$archive$resample_result(1)$learners[[1]]$model, "rpart")
  expect_rush_reset(instance$rush, type = "kill")
})

# test_that("crashing workers are detected", {
#   skip_on_cran()
#   skip_if_not_installed("rush")
#   flush_redis()

#   mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

#   instance = fsi_async(
#     task = tsk("pima"),
#     learner = lrn("classif.debug", segfault_train = 1),
#     resampling = rsmp("cv", folds = 3),
#     measures = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 10)
#   )

#   fselector = fs("async_random_search")

#   fselector$optimize(instance)
# })
