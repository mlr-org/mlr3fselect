context("FSelectInstance")

test_that("FSelect", {

  inst = TEST_MAKE_INST1(folds = 2L, measures = msr("classif.ce"), n_dim = 2L, term_evals = 5L)

  # Test empty instance
  expect_data_table(inst$bmr$data, nrows = 0)
  expect_identical(inst$n_evals, 0L)
  expect_identical(inst$n_batch, 0L)
  expect_output(print(inst), "Empty data.table")
  expect_error(inst$optimization_path())
  expect_error(inst$best())
  expect_list(inst$result)

  # Test instance
  z = inst$eval_batch(matrix(c(1,1,1,0,1,0,0,0), nrow = 2, ncol = 4))
  expect_data_table(inst$bmr$data, nrows = 4L)
  expect_identical(inst$n_evals, 2L)
  expect_list(z, len = 3)
  expect_named(z, c("batch_nr", "uhashes", "perf"))
  expect_equal(z$batch_nr, 1L)
  expect_character(z$uhashes, len = 2L)
  expect_data_table(z$perf, nrows = 2L, ncols = 1L)
  expect_named(z$perf, "classif.ce")
  expect_equal(inst$bmr$data$task[[1]]$feature_names, c("Petal.Length", "Petal.Width", "Sepal.Length")) # 1,1,1,0
  expect_vector(inst$bmr$data$task[[3]]$feature_names, c("Petal.Length")) # 1,0,0,0

  expect_data_table(inst$optimization_path(), nrows = 1)
  expect_data_table(inst$optimization_path(n = 2), nrows = 2)
  expect_r6(inst$best(), "ResampleResult")
  expect_data_table(inst$archive(), nrows = 2)
})

test_that("Budget",  {
  inst = TEST_MAKE_INST1(term_evals = 2L)
  expect_error(inst$eval_batch(diag(4)), class = "terminated_error")

  inst = TEST_MAKE_INST1(term_evals = 2L)
  fs = fs("random", batch_size = 6)
  fs$select(inst)
  tab = inst$archive()
  expect_data_table(tab, nrows = 6)

  fs$select(inst)
  tab = inst$archive()
  expect_data_table(tab, nrows = 6)
})
