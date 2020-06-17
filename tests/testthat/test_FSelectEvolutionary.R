context("FSelectEvolutionary")

test_that("FSelectEvolutionary", {
  z = test_fselect("evolutionary", mu = 4, lambda = 8, term_evals = 12)
})

test_that("FSelectEvolutionary - Initial solution", {
  z = test_fselect("evolutionary", mu = 4, lambda = 8,
    initial.solutions = list(c(1, 1, 1, 0)), term_evals = 12)
  r = z$inst$result_x_seach_space
  expect_equal(r, data.table(x1 = TRUE,
                       x2 = TRUE,
                       x3 = TRUE,
                       x4 = FALSE))
})

test_that("FSelectEvolutionary - Parent selector", {
  test_fselect("evolutionary", mu = 4, lambda = 8,
    parent.selector = "selRoulette", term_evals = 12)
  test_fselect("evolutionary", mu = 10, lambda = 8,
    parent.selector = "selGreedy", term_evals = 12)
})

test_that("FSelectEvolutionary - Survial selector", {
  test_fselect("evolutionary", mu = 4, lambda = 8,
    survival.selector = "selRoulette", term_evals = 12)
})

test_that("FSelectEvolutionary - Survial strategy", {
  test_fselect("evolutionary", mu = 4, lambda = 8, survival.strategy = "comma",
    n.elite = 1, term_evals = 12)
})

test_that("FSelectEvolutionary - Task with no features as inital solution", {
  test_fselect("evolutionary", mu = 4, lambda = 8,
    initial.solutions = list(c(0, 0, 0, 0)), term_evals = 10)
})
