context("FilterAUC")

test_that("FilterAUC", {
  f = FilterAUC$new()
  expect_filter(f, task = mlr_tasks$get("sonar"))
})
