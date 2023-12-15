test_that("importance is stored in the archive", {
  z = test_fselector("rfe", store_models = TRUE)
  a = z$inst$archive$data
  expect_names(names(z$inst$result), must.include = "importance")
  expect_character(z$inst$result$importance[[1]])
  expect_names(names(z$inst$archive$data), must.include = "importance")
  pwalk(a, function(x1, x2, x3, x4, importance, ...) expect_equal(x1 + x2 + x3 + x4, length(importance)))
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
  pwalk(a, function(x1, x2, x3, x4, importance, ...) expect_equal(x1 + x2 + x3 + x4, length(importance)))
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
  expect_error(test_fselector("rfe", subset_sizes = 40L), regexp = "Element 1 is not <= 4")
  expect_error(test_fselector("rfe", subset_sizes = c(3L, 1L, 2L)), regexp = "Must be sorted")
  expect_error(test_fselector("rfe", subset_sizes = c(1L, 2L, 3L)), regexp = "Must be sorted")
  expect_error(test_fselector("rfe", subset_sizes = 0L), regexp = "Element 1 is not >= 1")
})

test_that("subset is full feature set works", {
  z = test_fselector("rfe", feature_number = 4, store_models = TRUE)
  a = z$inst$archive$data
  expect_feature_number(a[batch_nr == 1, 1:4], n = 4)
})

test_that("learner without importance method throw an error", {
  learner = lrn("classif.rpart")
  learner$properties = setdiff(learner$properties, "importance")

  expect_error(fselect(
    fselector = fs("rfe"),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE
  ), "does not work with")
})

test_that("fix_importance function works", {
  learner = lrn("classif.rpart")
  task = tsk("pima")
  learner$train(task)
  feature_names = c("x", task$feature_names)

  importance = fix_importance(list(learner), feature_names)[[1]]
  expect_names(names(importance), permutation.of = feature_names)
  expect_equal(importance["x"], c(x = 0))

  importance = fix_importance(list(learner, learner), feature_names)
  walk(importance, function(x) expect_names(names(x), permutation.of = feature_names))
  walk(importance, function(x) expect_equal(x["x"], c(x = 0)))
})

test_that("raw_importance function works", {
  learner = lrn("classif.rpart")
  task = tsk("pima")
  learner$train(task)
  feature_names = task$feature_names

  importance = raw_importance(list(learner), feature_names)
  expect_numeric(importance)
  expect_names(names(importance), permutation.of = feature_names)
})

test_that("rank_importance function works", {
  learner = lrn("classif.rpart")
  task = tsk("pima")
  rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)
  feature_names = task$feature_names

  importance = rank_importance(rr$learners, feature_names)
  expect_numeric(importance)
  expect_names(names(importance), permutation.of = feature_names)
})

test_that("average_importance function works", {
  learner = lrn("classif.rpart")
  task = tsk("pima")
  rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)
  feature_names = task$feature_names

  importance = average_importance(rr$learners, feature_names)
  expect_numeric(importance)
  expect_names(names(importance), permutation.of = feature_names)
})

test_that("works without storing models", {
  optimizer = fs("rfe")
  expect_subset("requires_model", optimizer$properties)

  instance = fselect(
    fselector = fs("rfe"),
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = FALSE
  )

  expect_false(instance$objective$store_models)
  expect_numeric(instance$archive$data$importance[[1]])
  expect_null(instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model)
  expect_numeric(instance$archive$data$importance[[2]])
  expect_null(instance$archive$benchmark_result$resample_result(2)$learners[[1]]$model)

  instance = fselect(
    fselector = fs("rfe"),
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE
  )

  expect_true(instance$objective$store_models)
  expect_numeric(instance$archive$data$importance[[1]])
  expect_class(instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model, "rpart")
  expect_numeric(instance$archive$data$importance[[2]])
  expect_class(instance$archive$benchmark_result$resample_result(2)$learners[[1]]$model, "rpart")
})

#test_that("pipelines works", {
#  skip_if_not_installed("mlr3pipelines")
#  library("mlr3pipelines")
#
#  learner = as_learner(po("subsample") %>>% lrn("classif.rpart"))
#
#  instance = fselect(
#    fselector = fs("rfe"),
#    task = tsk("pima"),
#    learner = learner,
#    resampling = rsmp("holdout"),
#    measures = msr("classif.ce"),
#  )
#})

test_that("optimal features are selected with rank", {

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

  optimizer = fs("rfe", n_features = 1, feature_number = 1, aggregation = "rank")
  optimizer$optimize(instance)
  data = as.data.table(instance$archive)

  # number of features
  expect_feature_number(data[1, 1:4], n = 4)
  expect_feature_number(data[2, 1:4], n = 3)
  expect_feature_number(data[3, 1:4], n = 2)
  expect_feature_number(data[4, 1:4], n = 1)

  # features
  expect_best_features(instance$archive$best(batch = 1)[, 1:4], c("x1", "x2", "x3", "x4"))
  expect_best_features(instance$archive$best(batch = 2)[, 1:4], c("x2", "x3", "x4"))
  expect_best_features(instance$archive$best(batch = 3)[, 1:4], c("x2", "x3"))
  expect_best_features(instance$archive$best(batch = 4)[, 1:4], "x2")

  # importance
  expect_equal(data$importance[[1]], c(x2 = 4, x3 = 3, x4 = 2, x1 = 1))
  expect_equal(data$importance[[2]], c(x2 = 3, x3 = 2, x4 = 1))
  expect_equal(data$importance[[3]], c(x2 = 2, x3 = 1))
  expect_equal(data$importance[[4]], c(x2 = 1))

  expect_equal(instance$result$features[[1]], c("x2", "x3", "x4"))
})

test_that("optimal features are selected with mean", {

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

  optimizer = fs("rfe", n_features = 1, feature_number = 1, aggregation = "mean")
  optimizer$optimize(instance)
  data = as.data.table(instance$archive)

  # number of features
  expect_feature_number(data[1, 1:4], n = 4)
  expect_feature_number(data[2, 1:4], n = 3)
  expect_feature_number(data[3, 1:4], n = 2)
  expect_feature_number(data[4, 1:4], n = 1)

  # features
  expect_best_features(instance$archive$best(batch = 1)[, 1:4], c("x1", "x2", "x3", "x4"))
  expect_best_features(instance$archive$best(batch = 2)[, 1:4], c("x2", "x3", "x4"))
  expect_best_features(instance$archive$best(batch = 3)[, 1:4], c("x2", "x3"))
  expect_best_features(instance$archive$best(batch = 4)[, 1:4], "x2")

  # importance
  expect_equal(data$importance[[1]], c(x2 = 1.4, x3 = 1.2, x4 = 1.1, x1 = 0.8))
  expect_equal(data$importance[[2]], c(x2 = 1.4, x3 = 1.2, x4 = 1.1))
  expect_equal(data$importance[[3]], c(x2 = 1.4, x3 = 1.2))
  expect_equal(data$importance[[4]], c(x2 = 1.4))

  expect_equal(instance$result$features[[1]], c("x2", "x3", "x4"))
})
