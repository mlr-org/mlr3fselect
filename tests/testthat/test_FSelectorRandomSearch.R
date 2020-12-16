test_that("FSelectorRandomSearch", {
  test_fselector("random_search", batch_size = 10, term_evals = 2, real_evals = 10)
  test_fselector("random_search", batch_size = 4, term_evals = 4, real_evals = 4)

  z = test_fselector("random_search", max_features = 1, term_evals = 10)
  a = z$inst$archive$data
  expect_feature_number(a[, 1:4], n = 1)
})

test_that("FSelectorRandomSearch works with multi-crit", {
  test_fselector_2D("random_search", batch_size = 4, term_evals = 4, real_evals = 4)
})
