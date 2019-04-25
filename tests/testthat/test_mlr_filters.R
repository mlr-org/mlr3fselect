context("mlr_filters")

test_that("mlr_filters autotest", {
  keys = setdiff(mlr_filters$keys(), "variable_importance")

  task = mlr_tasks$get("sonar")
  for (key in keys) {
    f = mlr_filters$get(key)
    if (task$task_type %in% f$task_type) {
      f$calculate(task)
      expect_filter(f, task = task)
    }
  }

  task = mlr_tasks$get("mtcars")
  for (key in keys) {
    f = mlr_filters$get(key)
    if (task$task_type %in% f$task_type) {
      f$calculate(task)
      expect_filter(f, task = task)
    }
  }
})
