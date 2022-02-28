test_that("default parameters works", {
  z = test_fselector("sequential")
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 1)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 4)
})

test_that("sbs strategy works", {
  z = test_fselector("sequential", strategy = "sbs")
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 1)
})

test_that("sfs strategy works with max_features parameter", {
  z = test_fselector("sequential", max_features = 2)
  a = z$inst$archive$data
  expect_max_features(a[, 1:4], n = 2)
})

test_that("sbs strategy works with max_features parameter", {
  z = test_fselector("sequential", max_features = 2, strategy = "sbs")
  a = z$inst$archive$data
  expect_max_features(a[, 1:4], n = 2)
})

test_that("optimization_path method works", {
  z = test_fselector("sequential", store_models = TRUE)
  op = z$fselector$optimization_path(z$inst)
  expect_data_table(op, nrows = 4, ncols = 6)
  expect_equal(op$dummy, c(1, 2, 4, 3))
})

test_that("optimization_path method works with included uhash", {
  z = test_fselector("sequential", store_models = TRUE)
  op = z$fselector$optimization_path(z$inst, include_uhash = TRUE)
  expect_data_table(op)
  expect_names(names(op), must.include = "uhash")
  expect_equal(op$dummy, c(1, 2, 4, 3))
})
