context("FSelectRandom")

test_that("FSelectRandom", {
  test_fselect("random", batch_size = 10, term_evals = 2, real_evals = 10)
  test_fselect("random", batch_size = 4, term_evals = 4, real_evals = 4)

  z = test_fselect("random", max_features = 1, term_evals = 10)
  a = z$inst$archive()
  expect_features(a[, feat], n = 1)
})
