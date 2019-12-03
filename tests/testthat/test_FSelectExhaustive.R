context("FSelectExhaustive")

test_that("FSelectExhaustive", {
  test_fselect("exhaustive", term_evals = 2, real_evals = 4)
  test_fselect("exhaustive", term_evals = 4, real_evals = 4)
})
