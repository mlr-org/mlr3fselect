test_that("importance is stored in the archive", {
  z = test_fselector("rfe", store_models = TRUE)
  expect_names(names(z$inst$archive$data), must.include = "importance")
})

test_that("default parameters work", {
  z = test_fselector("rfe", store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 2)
})

test_that("recursive parameter works", {
  z = test_fselector("rfe", recursive = FALSE, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 2)
  expect_equal(a$importance[[1]][seq(2)], a$importance[[2]][seq(2)])
})

test_that("feature_fraction parameter works", {
  z = test_fselector("rfe", feature_fraction = 0.9, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)

  z = test_fselector("rfe", feature_fraction = 0, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)

  expect_error(test_fselector("rfe", feature_fraction = 1, store_models = TRUE), regexp = " Element 1 is not <=")

  z = test_fselector("rfe", recursive = FALSE, feature_fraction = 0.9, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)

  z = test_fselector("rfe", recursive = FALSE, feature_fraction = 0, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
})

test_that("feature_number parameter works", {
  z = test_fselector("rfe", feature_number = 1, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)

  z = test_fselector("rfe", recursive = FALSE, feature_number = 1, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 2)
})

test_that("subset_size parameter works", {
  z = test_fselector("rfe", subset_sizes = c(3L, 1L), store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 1)

  z = test_fselector("rfe", recursive = FALSE, subset_sizes = c(3L, 1L), store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
  expect_feature_number(a[batch_nr == 2, 1:4], n = 3)
  expect_feature_number(a[batch_nr == 3, 1:4], n = 1)

  expect_error(test_fselector("rfe", subset_sizes = c(2.5, 1)), regexp = "Must be of type 'integerish'")
  expect_error(test_fselector("rfe", subset_sizes = 40L), regexp = "Element 1 is not <= 3")
  expect_error(test_fselector("rfe", subset_sizes = c(3L, 1L, 2L)), regexp = "Must be sorted")
  expect_error(test_fselector("rfe", subset_sizes = c(1L, 2L, 3L)), regexp = "Must be sorted")
  expect_error(test_fselector("rfe", subset_sizes = 0L), regexp = "Element 1 is not >= 1")
})

test_that("learner without importance method throw an error", {
  learner = lrn("classif.rpart")
  learner$properties = setdiff(learner$properties, "importance")

  expect_error(fselect(
    method = "rfe",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    store_models = TRUE
  ), "does not work with")
})
