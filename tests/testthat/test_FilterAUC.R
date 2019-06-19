context("FilterAUC")

test_that("FilterAUC", {
  f = FilterAUC$new()
  expect_filter_result(f, task = mlr_tasks$get("sonar"))
})
