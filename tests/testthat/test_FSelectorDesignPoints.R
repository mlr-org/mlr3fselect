test_that("default parameters work", {
  design = data.table(
    x1 = c(TRUE, FALSE),
    x2 = c(TRUE, FALSE),
    x3 = c(FALSE, TRUE),
    x4 = c(FALSE, TRUE))

  z = test_fselector("design_points", design = design)
  a = z$inst$archive$data
  expect_equal(a[, 1:4], design)
})

test_that("multi-crit works", {
  design = data.table(
    x1 = c(TRUE, FALSE),
    x2 = c(TRUE, FALSE),
    x3 = c(FALSE, TRUE),
    x4 = c(FALSE, TRUE))

  z = test_fselector_2D("design_points", design = design)
  a = z$inst$archive$data
  expect_equal(a[, 1:4], design)
})
