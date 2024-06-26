test_that("empty FSelectInstanceBatchMultiCrit works", {
  inst = TEST_MAKE_INST_2D()

  expect_data_table(inst$archive$data, nrows = 0L)
  expect_identical(inst$archive$n_evals, 0L)
  expect_identical(inst$archive$n_batch, 0L)
  expect_null(inst$result)
})

test_that("eval_batch works", {
  inst = TEST_MAKE_INST_2D()

  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
    x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))

  z = inst$eval_batch(xdt)
  expect_named(z, c("regr.mse", "regr.rmse"))
  expect_identical(inst$archive$n_evals, 2L)
  expect_data_table(z, nrows = 2L)

  z = inst$eval_batch(xdt)
  expect_named(z, c("regr.mse", "regr.rmse"))
  expect_identical(inst$archive$n_evals, 4L)
  expect_data_table(z, nrows = 2L)
})

test_that("objective_function works", {
  inst = TEST_MAKE_INST_2D()
  y = inst$objective_function(c(1, 1, 0, 0))
  expect_named(y, c("regr.mse", "regr.rmse"))
})

test_that("store_benchmark_result flag works", {
  inst = TEST_MAKE_INST_2D(store_benchmark_result = FALSE)
  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
    x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))
  inst$eval_batch(xdt)

  expect_true("uhashes" %nin% colnames(inst$archive$data))

  inst = TEST_MAKE_INST_2D(store_benchmark_result = TRUE)
  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
                   x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))
  inst$eval_batch(xdt)
  expect_r6(inst$archive$benchmark_result, "BenchmarkResult")
})

test_that("result$features works", {
  inst = TEST_MAKE_INST_2D(store_benchmark_result = FALSE)
  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
    x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))
  ydt = data.table(regr.mse = list(0.1, 0.2),
    regr.rmse = list(0.2, 0.1))
  inst$assign_result(xdt, ydt)
  expect_list(inst$result_feature_set)
  expect_character(inst$result_feature_set[[1]])

  inst = TEST_MAKE_INST_2D(store_benchmark_result = FALSE)
  xdt = data.table(x1 = list(TRUE), x2 = list(FALSE),
    x3 = list(TRUE), x4 = list(TRUE))
  ydt = data.table(regr.mse = list(0.1),
    regr.rmse = list(0.2))
  inst$assign_result(xdt, ydt)
  expect_list(inst$result_feature_set)
  expect_character(inst$result_feature_set[[1]])
})
