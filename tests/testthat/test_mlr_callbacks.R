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
