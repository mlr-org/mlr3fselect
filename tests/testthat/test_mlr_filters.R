context("mlr_filters")

test_that("mlr_filters autotest", {
  keys = setdiff(mlr_filters$keys(), "variable_importance")

  task = mlr_tasks$get("sonar")
  for (key in keys) {
    f = mlr_filters$get(key)
    if (task$task_type %in% f$task_type) {
      f$calculate(task)
      expect_filter_result(f, task = task)
      new_task = f$filter_abs(task$clone(), 1)
      expect_string(new_task$feature_names)

      # try re-filtering
      f = mlr_filters$get(key)
      f$calculate(new_task)$filter_abs(new_task, 1)
    }
  }

  task = mlr_tasks$get("mtcars")
  for (key in keys) {
    f = mlr_filters$get(key)
    if (task$task_type %in% f$task_type) {
      f$calculate(task)
      expect_filter_result(f, task = task)
      new_task = f$filter_abs(task$clone(), 1)
      expect_string(new_task$feature_names)

      # try re-filtering
      f = mlr_filters$get(key)
      f$calculate(new_task)$filter_abs(new_task, 1)
    }
  }
})

test_that("sanity check regression", {
  gen = mlr_generators$get("friedman1")
  task = gen$generate(500)

  keys = as.data.table(mlr_filters)[map_lgl(task_type, is.element, el = "regr"), key]
  keys = setdiff(keys, "variance")
  for (key in keys) {
    f = mlr_filters$get(key)
    f$calculate(task)
    expect_true(startsWith(f$scores$feature[1], "important"))
  }
})
