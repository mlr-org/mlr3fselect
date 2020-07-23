context("FSelectorSequential")

test_that("FSelectorSequential", {
  test_fselector("sequential", term_evals = 2, real_evals = 4)
  test_fselector("sequential", term_evals = 10, real_evals = 10)

  z = test_fselector("sequential", term_evals = 10)
  a = z$inst$archive$data()
  expect_feature_number(a[batch_nr == 1, 1:4], n = 1)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 4)

  z = test_fselector("sequential", strategy = "sbs", term_evals = 10)
  a = z$inst$archive$data()
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 1)

  z = test_fselector("sequential", max_features = 2, term_evals = 7)
  a = z$inst$archive$data()
  expect_max_features(a[, 1:4], n = 2)

  z = test_fselector("sequential", max_features = 2, strategy = "sbs",
    term_evals = 8)
  a = z$inst$archive$data()
  expect_max_features(a[, 1:4], n = 2)
})

test_that("optimization_path method works", {
  z = test_fselector("sequential", term_evals = 10)
  op = z$fselector$optimization_path(z$inst)
  expect_data_table(op, nrows = 4)
  expect_equal(op$dummy, c(1, 2, 4, 3))
})
