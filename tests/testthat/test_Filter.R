context("Filter")

test_that("Errors for unsupported features", {
  # supported: numeric
  # supplied: factor, integer, numeric
  task = mlr3::mlr_tasks$get("boston_housing")
  filter = FilterLinearCorrelation$new()
  expect_error(filter$calculate(task))
})
