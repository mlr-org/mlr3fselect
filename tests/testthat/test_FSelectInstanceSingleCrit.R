context("FSelectInstanceSingleCrit")

test_that("empty FSelectInstanceSingleCrit works", {
  inst = TEST_MAKE_INST_1D()

  expect_data_table(inst$archive$data(), nrows = 0L)
  expect_identical(inst$archive$n_evals, 0L)
  expect_identical(inst$archive$n_batch, 0L)
  expect_null(inst$result)
})

test_that("eval_batch works", {
  inst = TEST_MAKE_INST_1D()

  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
    x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))

  z = inst$eval_batch(xdt)
  expect_equal(inst$archive$data()$resample_result[[1]]$task$feature_names,
    c("x1", "x3", "x4"))
  expect_equal(inst$archive$data()$resample_result[[2]]$task$feature_names,
    c("x2", "x3", "x4"))
  expect_identical(inst$archive$n_evals, 2L)
  expect_data_table(z, nrows = 2L)
  expect_named(z, c("dummy", "resample_result"))

  z = inst$eval_batch(xdt)
  expect_equal(inst$archive$data()$resample_result[[3]]$task$feature_names,
    c("x1", "x3", "x4"))
  expect_equal(inst$archive$data()$resample_result[[4]]$task$feature_names,
    c("x2", "x3", "x4"))
  expect_identical(inst$archive$n_evals, 4L)
  expect_data_table(z, nrows = 2L)
  expect_named(z, c("dummy", "resample_result"))

  a = inst$archive$data()
  expect_data_table(a, nrows = 4L)
})

test_that("objective_function works", {
  inst = TEST_MAKE_INST_1D()
  y = inst$objective_function(c(1, 1, 0, 0))
  expect_equal(y, c(dummy = -2))
})

test_that("store_resample_results flag works", {
  inst = TEST_MAKE_INST_1D(store_resample_results = FALSE)
  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
    x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))
  inst$eval_batch(xdt)

  expect_true("resample_result" %nin% colnames(inst$archive$data()))
})

test_that("result$features works",{
  inst = TEST_MAKE_INST_1D(store_resample_results = FALSE)
  xdt = data.table(x1 = list(TRUE), x2 = list(FALSE),
                   x3 = list(TRUE), x4 = list(TRUE))
  y = c(dummy = 2)

  inst$assign_result(xdt, y)
  expect_character(inst$result_feature_set)
})
