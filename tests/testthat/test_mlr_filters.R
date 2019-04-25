context("mlr_filters")

test_that("mlr_filters autotest", {
  task = mlr_tasks$get("sonar")

  for (key in mlr_filters$keys()) {
    f = mlr_filters$get(key)
    if (task$task_type %in% f$task_type) {
      f$calculate(task)
      expect_filter(f, task = task)
    }
  }

  # TODO: some FSelectorRcpp filters throw warnings!
  # task = mlr_tasks$get("mtcars")
  # for (key in mlr_filters$keys()) {
  #   f = mlr_filters$get(key)
  #   if (task$task_type %in% f$task_type) {
  #     f$calculate(task)
  #     expect_filter(f, task = task)
  #   }
  # }
})
