test_that("FSelectorSequentialPermutation", {
  z = test_fselector("sequential_permutation", term_evals = 30, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 1)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 4)
})