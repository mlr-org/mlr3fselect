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
