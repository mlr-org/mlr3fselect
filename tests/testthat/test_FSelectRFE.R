context("FSelectRFE")

test_that("FSelectRFE", {
  test_fselect("rfe", term_evals = 4L)
  test_fselect("rfe", min_features = 2L, term_evals = 3L)
  test_fselect("rfe", recursive = TRUE, term_evals = 4L)
  test_fselect("rfe", min_features = 2L, recursive = TRUE, term_evals = 3L)

  z = test_fselect("rfe", term_evals = 4L)
  a = z$inst$archive()
  expect_data_table(a, nrows = 4L)
})
