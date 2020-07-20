context("FSelectInstanceMultiCrit")

test_that("empty FSelectInstanceMultiCrit works", {
  inst = TEST_MAKE_INST_2D()

  expect_data_table(inst$archive$data(), nrows = 0L)
  expect_identical(inst$archive$n_evals, 0L)
  expect_identical(inst$archive$n_batch, 0L)
  expect_null(inst$result)
})

test_that("eval_batch works", {
  inst = TEST_MAKE_INST_2D()

  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
    x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))

  z = inst$eval_batch(xdt)
  expect_named(z, c("regr.mse", "regr.rmse", "resample_result"))
  expect_identical(inst$archive$n_evals, 2L)
  expect_data_table(z, nrows = 2L)

  z = inst$eval_batch(xdt)
  expect_named(z, c("regr.mse", "regr.rmse", "resample_result"))
  expect_identical(inst$archive$n_evals, 4L)
  expect_data_table(z, nrows = 2L)
})

test_that("objective_function works", {
  inst = TEST_MAKE_INST_2D()
  y = inst$objective_function(c(1, 1, 0, 0))
  expect_named(y, c("regr.mse", "regr.rmse"))
})
