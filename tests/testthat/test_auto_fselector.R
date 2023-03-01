test_that("auto_fselector function works", {
  afs = auto_fselector(fselector = fs("random_search", batch_size = 10), learner =  lrn("classif.rpart"), resampling = rsmp ("holdout"),
    measure = msr("classif.ce"), term_evals = 50)

  expect_class(afs, "AutoFSelector")
  expect_class(afs$instance_args$terminator, "TerminatorEvals")

  afs = auto_fselector(fselector = fs("random_search", batch_size = 10), learner =  lrn("classif.rpart"), resampling = rsmp ("holdout"),
    measure = msr("classif.ce"), term_time = 50)

  expect_class(afs, "AutoFSelector")
  expect_class(afs$instance_args$terminator, "TerminatorRunTime")

  afs = auto_fselector(fselector = fs("random_search", batch_size = 10), learner =  lrn("classif.rpart"), resampling = rsmp ("holdout"),
    measure = msr("classif.ce"), term_evals = 10, term_time = 50)

  expect_class(afs, "AutoFSelector")
  expect_class(afs$instance_args$terminator, "TerminatorCombo")
})
