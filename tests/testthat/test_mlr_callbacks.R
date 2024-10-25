test_that("backup callback works", {
  file = tempfile(fileext = ".rds")

  instance = fselect(
    fselector = fs("random_search", batch_size = 2),
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 4,
    callbacks = clbk("mlr3fselect.backup", path = file)
  )

  expect_file_exists(file)
  expect_benchmark_result(readRDS(file))
})

test_that("svm_rfe callbacks works", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("e1071")
  requireNamespace("mlr3learners")

  instance = fselect(
    fselector = fs("rfe", feature_number = 5, n_features = 10),
    task = tsk("sonar"),
    learner = lrn("classif.svm", type = "C-classification", kernel = "linear"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("none"),
    callbacks = clbk("mlr3fselect.svm_rfe"),
    store_models = TRUE
  )

  archive = as.data.table(instance$archive)
  expect_list(archive$importance, types = "numeric")
})

test_that("one_se_rule callback works", {

  score_design = data.table(
    score = c(0.1, 0.1, 0.58, 0.6),
    features = list("x1", c("x1", "x2"), c("x1", "x2", "x3"), c("x1", "x2", "x3", "x4"))
  )
  measure = msr("dummy", score_design = score_design)

  instance = fselect(
    fselector = fs("exhaustive_search"),
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = measure,
    callbacks = clbk("mlr3fselect.one_se_rule")
  )

  expect_equal(instance$result_feature_set, c("x1", "x2", "x3"))
})

test_that("internal tuning callback works", {
  learner = lrn("classif.debug", validate = "test", early_stopping = TRUE)

  internal_search_space = ps(
    iter = p_int(upper = 500, aggr = function(x) 233)
  )

  instance = fselect(
    fselector = fs("random_search"),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 10,
    callbacks = clbk("mlr3fselect.internal_tuning", internal_search_space = internal_search_space)
  )

  expect_data_table(instance$result)
  expect_names(names(instance$result), must.include = "internal_tuned_values")
  expect_equal(instance$result$internal_tuned_values[[1]], list(iter = 233))
})

test_that("internal tuning callback works with AutoFSelector", {
  learner = lrn("classif.debug", validate = "test", early_stopping = TRUE)

  internal_search_space = ps(
    iter = p_int(upper = 500, aggr = function(x) 233)
  )

  afs = auto_fselector(
    fselector = fs("random_search"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 10,
    callbacks = clbk("mlr3fselect.internal_tuning", internal_search_space = internal_search_space)
  )

  afs$train(tsk("pima"))

  expect_data_table(afs$fselect_instance$result)
  expect_names(names(afs$fselect_instance$result), must.include = "internal_tuned_values")
  expect_equal(afs$fselect_instance$result$internal_tuned_values[[1]], list(iter = 233))
  expect_equal(afs$model$learner$param_set$values$iter, 233)
})
