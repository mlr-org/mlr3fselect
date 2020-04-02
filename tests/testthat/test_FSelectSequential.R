context("FSelectSequential")

test_that("FSelectSequential", {
  test_fselect("sequential", term_evals = 2, real_evals = 4)
  test_fselect("sequential", term_evals = 10, real_evals = 10)

  z = test_fselect("sequential", term_evals = 10)
  a = z$inst$archive$data
  expect_features(a[batch_nr == 1, 1:4], n = 1)
  expect_features(a[batch_nr == 2, 1:4], n = 2)
  expect_features(a[batch_nr == 3, 1:4], n = 3)
  expect_features(a[batch_nr == 4, 1:4], n = 4)

  z = test_fselect("sequential", strategy = "sbs", term_evals = 10)
  a = z$inst$archive$data
  expect_features(a[batch_nr == 1, 1:4], n = 4)
  expect_features(a[batch_nr == 2, 1:4], n = 3)
  expect_features(a[batch_nr == 3, 1:4], n = 2)
  expect_features(a[batch_nr == 4, 1:4], n = 1)

  z = test_fselect("sequential", max_features = 2, term_evals = 7)
  a = z$inst$archive$data
  expect_features(a[,1:4], n = 2)

  z = test_fselect("sequential", max_features = 2, strategy = "sbs", term_evals = 8)
  a = z$inst$archive$data
  expect_features(a[,1:4], n = 2)
})
