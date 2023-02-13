test_that("extra columns are stored in the archive", {
  instance = fsi(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("dummy"),
    terminator = trm("none"),
    store_models = TRUE
  )

  optimizer = fs("rfecv", n_features = 1, feature_number = 1)
  optimizer$optimize(instance)

  expect_names(names(instance$archive$data), must.include = c("importance", "iteration"))
})

test_that("resampling is converted", {
  task = TEST_MAKE_TSK()
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  instance = fsi(
    task = task,
    learner = lrn("regr.rpart"),
    resampling = resampling,
    measures = msr("dummy"),
    terminator = trm("none"),
    store_models = TRUE
  )

  optimizer = fs("rfecv", n_features = 1, feature_number = 1)
  optimizer$optimize(instance)
  data = as.data.table(instance$archive)

  # check cv to custom
  walk(seq(3), function(i) {
    walk(data[list(i), resample_result, on = "iteration"], function(rr) {
      expect_class(rr$resampling, "ResamplingCustom")
      expect_equal(rr$resampling$train_set(1), resampling$train_set(i))
      expect_equal(rr$resampling$test_set(1), resampling$test_set(i))
    })
  })

  # check final run
  walk(data[is.na(iteration), resample_result], function(rr) {
    expect_class(rr$resampling, "ResamplingInsample")
  })
})

test_that("default parameters work", {
  instance = fsi(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("dummy"),
    terminator = trm("none"),
    store_models = TRUE
  )

  optimizer = fs("rfecv")
  optimizer$optimize(instance)
  data = as.data.table(instance$archive)

  walk(seq(3), function(i) {
    data = data[list(i), , on = "iteration"]
    expect_feature_number(data[1, 1:4], n = 4)
    expect_feature_number(data[2, 1:4], n = 2)
  })
})

test_that("learner without importance method throw an error", {
  learner = lrn("classif.rpart")
  learner$properties = setdiff(learner$properties, "importance")

  expect_error(fselect(
    method = fs("rfecv"),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE
  ), "does not work with")
})

test_that("optimal features are selected", {

  LearnerRegrDebugImportance = R6Class("LearnerRegrDebugImportance", inherit = LearnerRegrDebug,
    public = list(
      importance = function() {
        c(x2 = 1.4, x1 = 0.8, x3 = 1.2, x4 = 1.1)
      }
    )
  )

  learner = LearnerRegrDebugImportance$new()
  learner$properties = c(learner$properties, "importance")

  score_design = data.table(
    score = c(3, 4, 1, 2),
    features = list(
      c("x1", "x2", "x3", "x4"),
      c("x2", "x3", "x4"),
      c("x2", "x3"),
      "x2"))

  measure = msr("dummy", score_design = score_design)

  instance = fsi(
    task = TEST_MAKE_TSK(),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = measure,
    terminator = trm("none"),
    store_models = TRUE
  )

  optimizer = fs("rfecv", n_features = 1, feature_number = 1)
  optimizer$optimize(instance)
  data = as.data.table(instance$archive)

  # number of features in cv
  walk(seq(3), function(x) expect_feature_number(data[x, 1:4], n = 4))
  walk(4:6, function(x) expect_feature_number(data[x, 1:4], n = 3))
  walk(7:9, function(x) expect_feature_number(data[x, 1:4], n = 2))
  walk(10:12, function(x) expect_feature_number(data[x, 1:4], n = 1))

  # number of features in final run
  expect_feature_number(data[13, 1:4], n = 4)
  expect_feature_number(data[14, 1:4], n = 3)

  # features in cv
  expect_best_features(instance$archive$best(batch = 1)[, 1:4], c("x1", "x2", "x3", "x4"))
  expect_best_features(instance$archive$best(batch = 2)[, 1:4], c("x2", "x3", "x4"))
  expect_best_features(instance$archive$best(batch = 3)[, 1:4], c("x2", "x3"))
  expect_best_features(instance$archive$best(batch = 4)[, 1:4], "x2")

  # features in final run
  expect_best_features(instance$archive$best(batch = 5)[, 1:4], c("x1", "x2", "x3", "x4"))
  expect_best_features(instance$archive$best(batch = 6)[, 1:4], c("x2", "x3", "x4"))

  # importance
  expect_equal(data$importance[[1]], c(x2 = 1.4, x3 = 1.2, x4 = 1.1, x1 = 0.8))
  expect_equal(data$importance[[4]], c(x2 = 1.4, x3 = 1.2, x4 = 1.1))

  expect_equal(instance$result$features[[1]], c("x2", "x3", "x4"))
})
