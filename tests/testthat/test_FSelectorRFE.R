test_that("FSelectorRFE", {
  skip_on_cran()

  z = test_fselector("rfe", term_evals = 3L, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 1)
  expect_true("importance" %in% names(a))
  expect_equal(a$importance[[1]][1], a$importance[[3]][1])

  z = test_fselector("rfe", recursive = TRUE, term_evals = 3L, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 1)
  expect_true("importance" %in% names(a))

  z = test_fselector("rfe", feature_fraction = 0.9, term_evals = 3L,
    store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)
  expect_true("importance" %in% names(a))
  expect_equal(a$importance[[1]][1], a$importance[[3]][1])

  z = test_fselector("rfe", recursive = TRUE, feature_fraction = 0.9,
    term_evals = 4L, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 1)
  expect_true("importance" %in% names(a))

  z = test_fselector("rfe", feature_fraction = 0, term_evals = 1L,
    store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_true("importance" %in% names(a))

  z = test_fselector("rfe", feature_number = 1, term_evals = 4L,
    store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 1)
  expect_true("importance" %in% names(a))
  expect_equal(a$importance[[1]][1], a$importance[[3]][1])

  z = test_fselector("rfe", recursive = TRUE, feature_number = 1,
    term_evals = 4L, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 4, 1:4], n = 1)
  expect_true("importance" %in% names(a))

  z = test_fselector("rfe", subset_sizes = c(3L, 1L), term_evals = 3L,
    store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 1)
  expect_true("importance" %in% names(a))
  expect_equal(a$importance[[1]][1], a$importance[[3]][1])


  z = test_fselector("rfe", recursive = TRUE, subset_sizes = c(2L, 1L),
    term_evals = 3L, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 2)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 1)
  expect_true("importance" %in% names(a))

  expect_error(test_fselector("rfe", subset_sizes = c(2.5, 1L), term_evals = 3L),
    regexp = "Must be of type 'integerish'", fixed = TRUE)

  expect_error(test_fselector("rfe", subset_sizes = 40L, term_evals = 3L),
    regexp = "Element 1 is not <= 3", fixed = TRUE)

  expect_error(test_fselector("rfe", subset_sizes = c(3L, 1L, 2L), term_evals = 3L),
               regexp = "Must be sorted", fixed = TRUE)

  expect_error(test_fselector("rfe", subset_sizes = c(1L, 2L, 3L), term_evals = 3L),
               regexp = "Must be sorted", fixed = TRUE)

  expect_error(test_fselector("rfe", subset_sizes = 0L, term_evals = 3L),
               regexp = "Element 1 is not >= 1", fixed = TRUE)
})
