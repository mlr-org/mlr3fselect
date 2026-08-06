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
  z = test_fselector("sequential")
  op = z$fselector$optimization_path(z$inst)
  expect_data_table(op, nrows = 4, ncols = 6)
  expect_equal(op$dummy, c(1, 2, 4, 3))
})

test_that("optimization_path method works with included uhash", {
  z = test_fselector("sequential")
  op = z$fselector$optimization_path(z$inst, include_uhash = TRUE)
  expect_data_table(op)
  expect_names(names(op), must.include = "uhash")
  expect_equal(op$dummy, c(1, 2, 4, 3))
})

test_that("optimization_path method returns the best feature set of each batch", {
  score_design = data.table(
    score = c(1, 3, 5, 4),
    features = list("x3", c("x3", "x4"), c("x2", "x3", "x4"), c("x1", "x2", "x3", "x4"))
  )

  instance = fselect(
    fselector = fs("sequential"),
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("dummy", score_design = score_design, minimize = FALSE)
  )

  op = fs("sequential")$optimization_path(instance)
  expect_data_table(op, nrows = 4)
  expect_equal(op$dummy, c(1, 3, 5, 4))
  expect_equal(op$dummy[which.max(op$dummy)], instance$result_y[[1]])
})
