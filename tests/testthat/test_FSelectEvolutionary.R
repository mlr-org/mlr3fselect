context("FSelectEvolutionary")

test_that("FSelectEvolutionary", {
  test_fselect("evolutionary", mu = 5, term_evals = 2, real_evals = 2)
})
