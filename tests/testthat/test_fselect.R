test_that("fselect function works", {
  instance = fselect(method = "random_search", task = tsk("pima"), learner = lrn("classif.rpart"), resampling = rsmp ("holdout"), 
    measure = msr("classif.ce"), term_evals = 2, batch_size = 1)

  expect_class(instance, "FSelectInstanceSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})
