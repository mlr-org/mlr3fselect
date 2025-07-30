test_that("ArchiveAsyncFSelect access methods work", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE
  )

  fselector = fs("async_random_search")
  fselector$optimize(instance)

  # learner
  walk(seq(instance$rush$n_finished_tasks), function(i) {
    expect_learner(instance$archive$learner(i = i))
  })

  # learners
  walk(seq(instance$rush$n_finished_tasks), function(i) {
    expect_list(instance$archive$learners(i))
    expect_learner(instance$archive$learners(i)[[1]])
  })

  # predictions
  walk(seq(instance$rush$n_finished_tasks), function(i) {
    expect_list(instance$archive$predictions(i))
    expect_prediction(instance$archive$predictions(i)[[1]])
  })

  # resample result
  walk(seq(instance$rush$n_finished_tasks), function(i) {
    expect_resample_result(instance$archive$resample_result(i))
  })

  expect_benchmark_result(instance$archive$benchmark_result)
  expect_gte(instance$archive$benchmark_result$n_resample_results, 20L)
  expect_null(instance$archive$resample_result(1)$learners[[1]]$model)

  expect_rush_reset(instance$rush)
})

test_that("ArchiveAsyncFSelect as.data.table function works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE
  )
  fselector = fs("async_random_search")
  fselector$optimize(instance)

  # default
  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), must.include = c("age", "glucose", "insulin", "mass", "pedigree", "pregnant", "pressure", "triceps", "classif.ce", "runtime_learners", "timestamp_xs", "timestamp_ys", "warnings", "errors"))

  # extra measure
  tab = as.data.table(instance$archive, measures = msr("classif.acc"))
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), must.include = c("classif.acc"))

  # extra measures
  tab = as.data.table(instance$archive, measures = msrs(c("classif.acc", "classif.mcc")))
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), must.include = c("classif.acc", "classif.mcc"))

  # exclude column
  tab = as.data.table(instance$archive, exclude_columns = "timestamp_xs")
  expect_data_table(tab, min.rows = 20)
  expect_false("timestamp_xs" %in% names(tab))

  # exclude columns
  tab = as.data.table(instance$archive, exclude_columns = c("timestamp_xs", "resample_result"))
  expect_data_table(tab, min.rows = 20)
  expect_false(any(c("timestamp_xs", "resample_result") %in% names(tab)))

  # no exclude
  tab = as.data.table(instance$archive, exclude_columns = NULL)
  expect_data_table(tab, min.rows = 20)
  expect_true(all(c("timestamp_xs", "resample_result") %in% names(tab)))

  # no unnest
  tab = as.data.table(instance$archive, unnest = NULL)
  expect_data_table(tab, min.rows = 20)

  expect_rush_reset(instance$rush)
})

test_that("ArchiveAsyncFSelect as.data.table function works without resample result", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )
  fselector = fs("async_random_search")
  fselector$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20)
  expect_false("resample_result" %in% names(tab))

  expect_rush_reset(instance$rush)
})

test_that("ArchiveAsyncFSelect as.data.table function works with empty archive", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  expect_data_table(as.data.table(instance$archive), nrows = 0, ncols = 0)

  expect_rush_reset(instance$rush)
})

test_that("ArchiveAsyncFSelect as.data.table function works with multi-crit", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE
  )
  fselector = fs("async_random_search")
  fselector$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, min.rows = 20)
  expect_names(names(tab), must.include = c("classif.ce", "classif.acc"))

  expect_rush_reset(instance$rush)
})

test_that("ArchiveAsyncFSelect stores models if requested", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()
  on.exit({mirai::daemons(0)})

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

  expect_rush_reset(instance$rush)
})
