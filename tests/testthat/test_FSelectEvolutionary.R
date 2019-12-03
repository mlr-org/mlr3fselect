context("FSelectEvolutionary")

test_that("FSelectEvolutionary", {
  test_fselect("evolutionary", mu = 4, lambda = 8, term_evals = 2, real_evals = 2)

  test_fselect("evolutionary", mu = 4, lambda = 8, parent.selector = "selRoulette", term_evals = 12)
  test_fselect("evolutionary", mu = 4, lambda = 8, parent.selector = "selRanking", term_evals = 12)
  test_fselect("evolutionary", mu = 10, lambda = 8, parent.selector = "selGreedy", term_evals = 12)

  test_fselect("evolutionary", mu = 4, lambda = 8, survival.selector = "selRoulette", term_evals = 12)
  test_fselect("evolutionary", mu = 4, lambda = 8, survival.selector = "selRanking", term_evals = 12)
  test_fselect("evolutionary", mu = 4, lambda = 8, survival.selector = "selRanking", term_evals = 12)

  test_fselect("evolutionary", mu = 4, lambda = 8, survival.strategy = "comma",  term_evals = 12)
  test_fselect("evolutionary", mu = 4, lambda = 8, survival.strategy = "comma", n.elite = 1,  term_evals = 12)

  test_fselect("evolutionary", mu = 4, lambda = 8, initial.solutions = list(c(0,0,0,0)), term_evals = 10)
})



