context("FSelectRandomBinary")

test_that("FSelectRandomBinary", {
  test_fselect("random_binary", batch_size = 10, term_evals = 2, real_evals = 10)
  test_fselect("random_binary", batch_size = 4, term_evals = 4, real_evals = 4)

  z = test_fselect("random_binary", max_features = 1, term_evals = 10)
  a = z$inst$archive$data()
  expect_features(a[, 1:4], n = 1)
})
