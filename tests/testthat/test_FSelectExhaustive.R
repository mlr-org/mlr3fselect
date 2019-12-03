context("FSelectExhaustive")

test_that("FSelectExhaustive", {
  test_fselect("exhaustive", term_evals = 2, real_evals = 4)
  test_fselect("exhaustive", term_evals = 4, real_evals = 4)

  z = test_fselect("exhaustive", term_evals = 15)
  a = z$inst$archive()
  expect_features(a[batch_nr == 1, feat], n = 1)
  expect_features(a[batch_nr == 2, feat], n = 2)
  expect_features(a[batch_nr == 3, feat], n = 3)
  expect_features(a[batch_nr == 4, feat], n = 4)

  z = test_fselect("exhaustive", max_features = 2, term_evals = 10)
  a = z$inst$archive()
  expect_features(a[, feat], n = 2)
})
