test_that("default parameters work", {
  z = test_fselector("exhaustive_search", store_models = TRUE)
  a = z$inst$archive$data

  expect_feature_number(a[batch_nr == 1, 1:4], n = 1)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 4)
  r = z$inst$result_x_search_space
  expect_equal(r, data.table(x1 = TRUE, x2 = TRUE, x3 = TRUE, x4 = FALSE))
})

test_that("max_features parameter works", {
  z = test_fselector("exhaustive_search", max_features = 2, store_models = TRUE)
  a = z$inst$archive$data

  expect_max_features(a[, 1:4], n = 2)
  r = z$inst$result_x_search_space
  expect_equal(r, data.table(x1 = TRUE, x2 = TRUE, x3 = FALSE, x4 = FALSE))
})

test_that("multi-crit works", {
  test_fselector_2D("exhaustive_search")
})
