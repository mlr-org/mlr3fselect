context("FSelectorExhaustive")

test_that("FSelectorExhaustive", {
  test_fselector("exhaustive_search", term_evals = 2, real_evals = 4)
  test_fselector("exhaustive_search", term_evals = 4, real_evals = 4)

  z = test_fselector("exhaustive_search", term_evals = 15)
  a = z$inst$archive$data()
  expect_feature_number(a[batch_nr == 1, 1:4], n = 1)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 4)
  r = z$inst$result_x_search_space
  expect_equal(r, data.table(x1 = TRUE,
    x2 = TRUE,
    x3 = TRUE,
    x4 = FALSE))

  z = test_fselector("exhaustive_search", max_features = 2, term_evals = 10)
  a = z$inst$archive$data()
  expect_max_features(a[, 1:4], n = 2)
  r = z$inst$result_x_search_space
  expect_equal(r, data.table(x1 = TRUE,
    x2 = TRUE,
    x3 = FALSE,
    x4 = FALSE))
})
