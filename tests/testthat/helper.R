library(checkmate)
library(mlr3)
lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_filter = function(f, task = NULL) {
  expect_r6(f, "Filter",
    public = c("packages", "feature_types", "task_type", "param_set", "filter_values"),
    private = c(".calculate")
  )

  expect_character(f$packages, any.missing = FALSE, unique = TRUE)
  expect_subset(f$task_type, mlr_reflections$task_types)
  expect_subset(f$feature_types, mlr_reflections$task_feature_types)
  expect_class(f$param_set, "ParamSet")
  expect_function(private(f)$.calculate, args = "task")

  if (!is.null(f$filter_values)) {
    expect_numeric(f$filter_values, any.missing = FALSE, names = "unique")
    expect_false(is.unsorted(rev(f$filter_values)))
  }

  if (!is.null(task)) {
    x = f$clone(deep = TRUE)$calculate(task)
    expect_class(x, "Filter")
    expect_numeric(x$filter_values, any.missing = FALSE, names = "unique")
    expect_false(is.unsorted(rev(x$filter_values)))
  }
}
