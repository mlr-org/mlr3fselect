test_that("default parameters work", {
  test_fselector("random_search", batch_size = 5, term_evals = 10)
})

test_that("max_features parameter work", {
  z = test_fselector("random_search", max_features = 1, term_evals = 10)
  a = z$inst$archive$data
  expect_feature_number(a[, 1:4], n = 1)
})

test_that("multi-crit works", {
  test_fselector_2D("random_search", batch_size = 5, term_evals = 10)
})
