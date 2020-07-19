context("FSelectorDesignPoints")

test_that("FSelectorRandomSearch", {
  design = data.table(
    x1 = c(TRUE, FALSE),
    x2 = c(TRUE, FALSE),
    x3 = c(FALSE, TRUE),
    x4 = c(FALSE, TRUE))

  z = test_fselector("design_points", design = design, batch_size = 10, term_evals = 10, real_evals = 2)
  a = z$inst$archive$data()
  expect_equal(a[,1:4], design)
})
