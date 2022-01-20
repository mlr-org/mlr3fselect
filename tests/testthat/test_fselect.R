test_that("fselect function works with single measure", {
  instance = fselect(method = "random_search", task = tsk("pima"), learner = lrn("classif.rpart"), resampling = rsmp ("holdout"),
    measures = msr("classif.ce"), term_evals = 2, batch_size = 1)

  expect_class(instance, "FSelectInstanceSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})

test_that("fselect function works with multiple measures", {
  instance = fselect(method = "random_search", task = tsk("pima"), learner = lrn("classif.rpart"), resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")), term_evals = 2, batch_size = 1)

  expect_class(instance, "FSelectInstanceMultiCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})
