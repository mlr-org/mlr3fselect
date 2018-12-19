test_that("Errors for unsupported features", {

  # suppported: numeric
  # supplied: factor, integer, numeric
  task = mlr_tasks$get("bh")
  filter = FilterLinearCorrelation$new()
  expect_error(filter$calculate(task))

})
