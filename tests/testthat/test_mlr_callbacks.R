test_that("backup callback works", {
  file = tempfile(fileext = ".rds")

  instance = fselect(
    method = "random_search",
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 4,
    batch_size = 2,
    callbacks = clbk("mlr3fselect.backup", path = file)
  )

  expect_file_exists(file)
  expect_benchmark_result(readRDS(file))
})
