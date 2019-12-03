context("FSelectSequential")

test_that("FSelectSequential", {
  test_fselect("sequential", term_evals = 2, real_evals = 4)
  test_fselect("sequential", term_evals = 10, real_evals = 10)
})
