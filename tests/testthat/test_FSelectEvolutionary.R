context("FSelectEvolutionary")

test_that("FSelectEvolutionary", {
  test_fselect("evolutionary", mu = 4, lambda = 8, term_evals = 12)

  z = test_fselect("evolutionary", mu = 4, lambda = 8, initial.solutions = list(c(1,1,1,0)), term_evals = 12)
  a = z$inst$archive()
  expect_data_table(a, nrows = 12L)
  r = z$inst$result
  expect_equal(r$feat, c("Petal.Length", "Petal.Width", "Sepal.Length"))

  test_fselect("evolutionary", mu = 4, lambda = 8, parent.selector = "selRoulette", term_evals = 12)
  test_fselect("evolutionary", mu = 10, lambda = 8, parent.selector = "selGreedy", term_evals = 12)

  test_fselect("evolutionary", mu = 4, lambda = 8, survival.selector = "selRoulette", term_evals = 12)

  test_fselect("evolutionary", mu = 4, lambda = 8, survival.strategy = "comma",  term_evals = 12)
  test_fselect("evolutionary", mu = 4, lambda = 8, survival.strategy = "comma", n.elite = 1,  term_evals = 12)

  test_fselect("evolutionary", mu = 4, lambda = 8, initial.solutions = list(c(0,0,0,0)), term_evals = 10)
})



