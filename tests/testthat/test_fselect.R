
test_that("auto_fselector function works", {
  afs = auto_fselector(method = "random_search", learner =  lrn("classif.rpart"), resampling = rsmp ("holdout"), 
    measure = msr("classif.ce"), term_evals = 50, batch_size = 10)

  expect_class(afs, "AutoFSelector")
  expect_class(afs$instance_args$terminator, "TerminatorEvals")

  afs = auto_fselector(method = "random_search", learner =  lrn("classif.rpart"), resampling = rsmp ("holdout"), 
    measure = msr("classif.ce"), term_time = 50, batch_size = 10)

  expect_class(afs, "AutoFSelector")
  expect_class(afs$instance_args$terminator, "TerminatorRunTime")

  afs = auto_fselector(method = "random_search", learner =  lrn("classif.rpart"), resampling = rsmp ("holdout"), 
    measure = msr("classif.ce"), term_evals = 10, term_time = 50, batch_size = 10)

  expect_class(afs, "AutoFSelector")
  expect_class(afs$instance_args$terminator, "TerminatorCombo")
})

test_that("fselect function works", {
  instance = fselect(method = "random_search", task = tsk("pima"), learner = lrn("classif.rpart"), resampling = rsmp ("holdout"), 
    measure = msr("classif.ce"), term_evals = 2, batch_size = 1)

  expect_class(instance, "FSelectInstanceSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})
