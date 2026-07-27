test_that("mlr_fselectors", {
  expect_dictionary(mlr_fselectors, min_items = 1L)
  keys = mlr_fselectors$keys()

  for (key in keys) {
    fselector = fs(key)
    expect_r6(fselector, "FSelector")
  }
})

test_that("mlr_fselectors sugar", {
  expect_class(fs("random_search"), "FSelector")
  expect_class(fss(c("random_search", "random_search")), "list")
})

test_that("as.data.table objects parameter", {
  tab = as.data.table(mlr_fselectors, objects = TRUE)
  expect_data_table(tab)
  expect_list(tab$object, "FSelector", any.missing = FALSE)
})

test_that("reloading the package does not duplicate reflections", {
  bbotk_reflections = utils::getFromNamespace("bbotk_reflections", ns = "bbotk")
  mlr_reflections = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

  register_reflections()
  register_reflections()

  expect_equal(sum(bbotk_reflections$optimizer_properties == "requires_model"), 1L)
  expect_equal(sum(mlr_reflections$loaded_packages == "mlr3fselect"), 1L)
  walk(mlr_reflections$task_col_roles, function(col_roles) {
    expect_equal(sum(col_roles == "always_included"), 1L)
  })
})
