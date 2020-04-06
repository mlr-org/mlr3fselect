context("FSelectExhaustive")

test_that("FSelectExhaustive", {
  test_fselect("exhaustive", term_evals = 2, real_evals = 4)
  test_fselect("exhaustive", term_evals = 4, real_evals = 4)

  z = test_fselect("exhaustive", term_evals = 15)
  a = z$inst$archive$data
  expect_features(a[batch_nr == 1, 1:4], n = 1)
  expect_features(a[batch_nr == 2, 1:4], n = 2)
  expect_features(a[batch_nr == 3, 1:4], n = 3)
  expect_features(a[batch_nr == 4, 1:4], n = 4)
  r = z$inst$result
  expect_equal(r$feat, c("x1", "x2", "x3"))

  z = test_fselect("exhaustive", max_features = 2, term_evals = 10)
  a = z$inst$archive$data
  expect_features(a[, 1:4], n = 2)
  expect_data_table(a, nrows = 10L)
  r = z$inst$result
  expect_equal(r$feat, c("x1", "x2"))
})
