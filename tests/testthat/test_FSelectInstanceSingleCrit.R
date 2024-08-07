test_that("empty FSelectInstanceBatchSingleCrit works", {
  inst = TEST_MAKE_INST_1D()

  expect_data_table(inst$archive$data, nrows = 0L)
  expect_identical(inst$archive$n_evals, 0L)
  expect_identical(inst$archive$n_batch, 0L)
  expect_null(inst$result)
})

test_that("eval_batch works", {
  inst = TEST_MAKE_INST_1D()

  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
    x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))

  z = inst$eval_batch(xdt)
  expect_equal(inst$archive$benchmark_result$resample_result(1)$task$feature_names,
    c("x1", "x3", "x4"))
  expect_equal(inst$archive$benchmark_result$resample_result(2)$task$feature_names,
    c("x2", "x3", "x4"))
  expect_identical(inst$archive$n_evals, 2L)
  expect_data_table(z, nrows = 2L)
  expect_named(z, "dummy")

  z = inst$eval_batch(xdt)
  expect_equal(inst$archive$benchmark_result$resample_result(3)$task$feature_names,
    c("x1", "x3", "x4"))
  expect_equal(inst$archive$benchmark_result$resample_result(4)$task$feature_names,
    c("x2", "x3", "x4"))
  expect_identical(inst$archive$n_evals, 4L)
  expect_data_table(z, nrows = 2L)
  expect_named(z, "dummy")

  a = inst$archive$data
  expect_data_table(a, nrows = 4L)
})

test_that("objective_function works", {
  inst = TEST_MAKE_INST_1D()
  y = inst$objective_function(c(1, 1, 0, 0))
  expect_equal(y, c(dummy = -2))
})

test_that("store_benchmark_result flag works", {
  inst = TEST_MAKE_INST_1D(store_benchmark_result = FALSE)
  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
    x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))
  inst$eval_batch(xdt)

  expect_true("uhashes" %nin% colnames(inst$archive$data))

  inst = TEST_MAKE_INST_1D(store_benchmark_result = TRUE)
  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
                   x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))
  inst$eval_batch(xdt)

  expect_r6(inst$archive$benchmark_result, "BenchmarkResult")
})

test_that("result$features works", {
  inst = TEST_MAKE_INST_1D(store_benchmark_result = FALSE)
  xdt = data.table(x1 = list(TRUE), x2 = list(FALSE),
    x3 = list(TRUE), x4 = list(TRUE))
  y = c(dummy = 2)

  inst$assign_result(xdt, y)
  expect_character(inst$result_feature_set)
})

test_that("always include variable works", {
  task = tsk("pima")
  task$set_col_roles("glucose", "always_included")

  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  instance = fselect(
    fselector = fs("random_search", batch_size = 100),
    task = task,
    learner = learner,
    resampling = resampling,
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 100),
    store_models = TRUE
  )

  data = as.data.table(instance$archive)

  expect_names(instance$archive$cols_x, disjunct.from = "gloucose")
  expect_names(names(instance$archive$data), disjunct.from = "gloucose")
  walk(data$resample_result, function(rr) {
    expect_names(names(rr$learners[[1]]$state$data_prototype) %??% rr$learners[[1]]$state$feature_names, must.include = "glucose")
  })
})

test_that("always include variables works", {
  task = tsk("pima")
  task$set_col_roles(c("glucose", "age"), "always_included")

  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  instance = fselect(
    fselector = fs("random_search", batch_size = 100),
    task = task,
    learner = learner,
    resampling = resampling,
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 100),
    store_models = TRUE
  )

  data = as.data.table(instance$archive)

  expect_names(instance$archive$cols_x, disjunct.from = c("glucose", "age"))
  expect_names(names(instance$archive$data), disjunct.from = c("glucose", "age"))
  walk(data$resample_result, function(rr) {
    expect_names(names(rr$learners[[1]]$state$data_prototype) %??% rr$learners[[1]]$state$feature_names, must.include = c("glucose", "age"))
  })
})

test_that("objective contains no benchmark results", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 10)

  instance = fsi(task, learner, resampling, measure, terminator)
  fselector = fs("random_search", batch_size = 1)
  fselector$optimize(instance)

  expect_null(instance$objective$.__enclos_env__$private$.benchmark_result)
})
