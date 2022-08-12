skip_if_not_installed("genalg")

test_that("default parameters work", {
  test_fselector("genetic_search", term_evals = 10)
})
