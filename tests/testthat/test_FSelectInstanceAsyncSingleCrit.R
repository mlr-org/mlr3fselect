skip_if_not_installed("rush")
skip_if_no_redis()

test_that("initializing FSelectInstanceAsyncSingleCrit works", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  expect_r6(instance$archive, "ArchiveAsyncFSelect")
  expect_r6(instance$objective, "Objective")
  expect_r6(instance$search_space, "ParamSet")
  expect_r6(instance$terminator, "Terminator")
  expect_r6(instance$rush, "Rush")
  expect_null(instance$result)
})

test_that("rush controller can be passed to FSelectInstanceAsyncSingleCrit", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  expect_class(instance$rush, "Rush")
})

test_that("FSelectInstanceAsyncSingleCrit can be passed to a fselector", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  fselector = fs("async_random_search")
  fselector$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 3L)
})

test_that("assigning a result to FSelectInstanceAsyncSingleCrit works", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  fselector = fs("async_random_search")
  fselector$optimize(instance)

  result = instance$result
  expect_data_table(result, nrows = 1)
  expect_names(names(result), must.include = c("features", "n_features", "classif.ce"))
})

test_that("saving the benchmark result with FSelectInstanceAsyncSingleCrit works", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    rush = rush
  )

  fselector = fs("async_random_search")
  fselector$optimize(instance)

  expect_benchmark_result(instance$archive$benchmark_result)
  expect_gte(instance$archive$benchmark_result$n_resample_results, 3L)
  expect_null(instance$archive$resample_result(1)$learners[[1]]$model)
})

test_that("saving the models with FSelectInstanceAsyncSingleCrit works", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_benchmark_result = TRUE,
    store_models = TRUE,
    rush = rush
  )

  fselector = fs("async_random_search")
  fselector$optimize(instance)

  expect_benchmark_result(instance$archive$benchmark_result)
  expect_gte(instance$archive$benchmark_result$n_resample_results, 3L)
  expect_class(instance$archive$resample_result(1)$learners[[1]]$model, "rpart")
})

# test_that("crashing workers are detected", {
#   rush = start_rush()
#   on.exit({
#     rush$reset()
#     mirai::daemons(0)
#   })

#   instance = fsi_async(
#     task = tsk("pima"),
#     learner = lrn("classif.debug", segfault_train = 1),
#     resampling = rsmp("cv", folds = 3),
#     measures = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 10),
#     rush = rush
#   )

#   fselector = fs("async_random_search")

#   fselector$optimize(instance)
# })
