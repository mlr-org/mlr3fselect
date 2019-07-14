context("Filter")

test_that("Errors for unsupported features", {
  # supported: numeric
  # supplied: factor, integer, numeric
  task = mlr3::mlr_tasks$get("boston_housing")
  filter = FilterCorrelation$new()
  expect_error(filter$calculate(task))
})

test_that("fr$combine()", {
  task = mlr3::mlr_tasks$get("iris")

  # create filters
  fr1 = FilterMIM$new()
  fr1$calculate(task)
  fr2 = FilterVariance$new()
  fr2$calculate(task)

  expect_data_table(fr1$scores, nrows = 4, ncols = 3)
  expect_data_table(fr2$scores, nrows = 4, ncols = 3)

  fr2$combine(fr1)

  expect_data_table(fr2$scores, nrows = 8, ncols = 3)

  expect_filter_result(fr1)
  expect_filter_result(fr2)
})
