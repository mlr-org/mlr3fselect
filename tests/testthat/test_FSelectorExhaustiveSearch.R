test_that("default parameters work", {
  z = test_fselector("exhaustive_search")
  a = z$inst$archive$data

  expect_feature_number(a[seq(4), list(x1, x2, x3, x4)], n = 1)
  expect_feature_number(a[6:10, list(x1, x2, x3, x4)], n = 2)
  expect_feature_number(a[11:14, list(x1, x2, x3, x4)], n = 3)
  expect_feature_number(a[15, list(x1, x2, x3, x4)], n = 4)
  r = z$inst$result_x_search_space
  expect_equal(r, data.table(x1 = TRUE, x2 = TRUE, x3 = TRUE, x4 = FALSE))
})

test_that("max_features parameter works", {
  z = test_fselector("exhaustive_search", max_features = 2)
  a = z$inst$archive$data

  expect_max_features(a[,list(x1, x2, x3, x4)], n = 2)
  r = z$inst$result_x_search_space
  expect_equal(r, data.table(x1 = TRUE, x2 = TRUE, x3 = FALSE, x4 = FALSE))
})

test_that("multi-crit works", {
  test_fselector_2D("exhaustive_search")
})

test_that("batch_size parameter works", {
  z = test_fselector("exhaustive_search", batch_size = 2)
  a = z$inst$archive$data

  expect_data_table(a[list(1), , on = "batch_nr"], nrows = 2)
})
