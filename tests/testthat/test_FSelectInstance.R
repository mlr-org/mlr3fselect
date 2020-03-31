context("FSelectInstance")

test_that("FSelect", {
  inst = TEST_MAKE_INST1(folds = 2L, measures = msr("classif.ce"), n_dim = 2L, term_evals = 5L)

  # Test empty instance
  expect_data_table(inst$evaluator$archive$data, nrows = 0)
  expect_identical(inst$evaluator$archive$n_evals, 0L)
  expect_identical(inst$evaluator$archive$n_batch, 0L)
  expect_list(inst$result)
})

test_that("Budget",  {
  inst = TEST_MAKE_INST1(term_evals = 2L)
  expect_error(inst$eval_batch(diag(4)), class = "terminated_error")

  inst = TEST_MAKE_INST1(term_evals = 2L)
  fs = fs("random", batch_size = 6)
  fs$select(inst)
  tab = inst$evaluator$archive$data
  expect_data_table(tab, nrows = 6)

  fs$select(inst)
  tab = inst$evaluator$archive$data
  expect_data_table(tab, nrows = 6)
})
