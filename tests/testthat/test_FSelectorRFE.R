context("FSelectorRFE")

test_that("FSelectorRFE", {
  z = test_fselector("rfe", term_evals = 4L, store_models = TRUE)
  a = z$inst$archive$data()
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 1)

  test_fselector("rfe", min_features = 2L, term_evals = 3L, store_models = TRUE)
  a = z$inst$archive$data()
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)

  test_fselector("rfe", recursive = TRUE, term_evals = 4L, store_models = TRUE)
  a = z$inst$archive$data()
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 1)

  test_fselector("rfe", min_features = 2L, recursive = TRUE, term_evals = 3L,
    store_models = TRUE)
  a = z$inst$archive$data()
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)
})
