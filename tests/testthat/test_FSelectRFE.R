context("FSelectRFE")

test_that("FSelectRFE", {
  test_fselect("rfe", term_evals = 4L, store_models = TRUE)
  test_fselect("rfe", min_features = 2L, term_evals = 3L, store_models = TRUE)
  test_fselect("rfe", recursive = TRUE, term_evals = 4L, store_models = TRUE)
  test_fselect("rfe", min_features = 2L, recursive = TRUE, term_evals = 3L,
    store_models = TRUE)

  z = test_fselect("rfe", term_evals = 4L, store_models = TRUE)
  a = z$inst$archive$data
  expect_data_table(a, nrows = 4L)
})
