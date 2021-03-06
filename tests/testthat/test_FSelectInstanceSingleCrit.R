test_that("empty FSelectInstanceSingleCrit works", {
  inst = TEST_MAKE_INST_1D()

  expect_data_table(inst$archive$data, nrows = 0L)
  expect_identical(inst$archive$n_evals, 0L)
  expect_identical(inst$archive$n_batch, 0L)
  expect_null(inst$result)
})

test_that("eval_batch works", {
  inst = TEST_MAKE_INST_1D(store_models = TRUE)

  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
    x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))

  z = inst$eval_batch(xdt)
  expect_equal(inst$archive$benchmark_result$resample_result(1)$learners[[1]]$model$select$selection,
    c("x1", "x3", "x4"))
  expect_equal(inst$archive$benchmark_result$resample_result(2)$learners[[1]]$model$select$selection,
    c("x2", "x3", "x4"))
  expect_identical(inst$archive$n_evals, 2L)
  expect_data_table(z, nrows = 2L)
  expect_named(z, "dummy")

  z = inst$eval_batch(xdt)
  expect_equal(inst$archive$benchmark_result$resample_result(3)$learners[[1]]$model$select$selection,
    c("x1", "x3", "x4"))
  expect_equal(inst$archive$benchmark_result$resample_result(4)$learners[[1]]$model$select$selection,
    c("x2", "x3", "x4"))
  expect_identical(inst$archive$n_evals, 4L)
  expect_data_table(z, nrows = 2L)
  expect_named(z, "dummy")

  a = inst$archive$data
  expect_data_table(a, nrows = 4L)
})

test_that("objective_function works", {
  inst = TEST_MAKE_INST_1D(store_models = TRUE)
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
