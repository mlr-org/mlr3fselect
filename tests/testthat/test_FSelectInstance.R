context("FSelectInstance")

test_that("FSelectInstance - Empty", {
  inst = TEST_MAKE_INST(term_evals = 5L)

  expect_data_table(inst$archive$data(), nrows = 0L)
  expect_identical(inst$archive$n_evals, 0L)
  expect_identical(inst$archive$n_batch, 0L)
  expect_null(inst$result)
})

test_that("FSelectInstance - Eval batch", {
  inst = TEST_MAKE_INST(term_evals = 5L)

  xdt = data.table(x1 = list(TRUE, FALSE), x2 = list(FALSE, TRUE),
    x3 = list(TRUE, TRUE), x4 = list(TRUE, TRUE))

  z = inst$eval_batch(xdt)
  expect_equal(inst$archive$data()$resample_result[[1]]$task$feature_names,
    c("x1", "x3", "x4"))
  expect_equal(inst$archive$data()$resample_result[[2]]$task$feature_names,
    c("x2", "x3", "x4"))
  expect_identical(inst$archive$n_evals, 2L)
  expect_data_table(z, nrows = 2L)
  expect_named(z, c("dummy.sequential", "resample_result"))

  z = inst$eval_batch(xdt)
  expect_equal(inst$archive$data()$resample_result[[3]]$task$feature_names,
    c("x1", "x3", "x4"))
  expect_equal(inst$archive$data()$resample_result[[4]]$task$feature_names,
    c("x2", "x3", "x4"))
  expect_identical(inst$archive$n_evals, 4L)
  expect_data_table(z, nrows = 2L)
  expect_named(z, c("dummy.sequential", "resample_result"))

  a = inst$archive$data()
  expect_data_table(a, nrows = 4L)
})

test_that("Budget", {
  inst = TEST_MAKE_INST(term_evals = 2L)
  fs = fs("random", batch_size = 6)
  fs$optimize(inst)
  tab = inst$archive$data
  expect_data_table(tab, nrows = 6)

  fs$optimize(inst)
  tab = inst$archive$data
  expect_data_table(tab, nrows = 6)
})
