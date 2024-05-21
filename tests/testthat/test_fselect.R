test_that("fselect function works with single measure", {
  instance = fselect(fselector = fs("random_search", batch_size = 1), task = tsk("pima"), learner = lrn("classif.rpart"), resampling = rsmp ("holdout"),
    measures = msr("classif.ce"), term_evals = 2)

  expect_class(instance, "FSelectInstanceBatchSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})

test_that("fselect function works with multiple measures", {
  instance = fselect(fselector = fs("random_search", batch_size = 1), task = tsk("pima"), learner = lrn("classif.rpart"), resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")), term_evals = 2)

  expect_class(instance, "FSelectInstanceBatchMultiCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})

test_that("fselect function accepts string input for method", {
  instance = fselect(fselector = fs("random_search", batch_size = 1), task = tsk("pima"), learner = lrn("classif.rpart"), resampling = rsmp ("holdout"),
    measures = msr("classif.ce"), term_evals = 2)

  expect_class(instance, "FSelectInstanceBatchSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})

test_that("fselect interface is equal to FSelectInstanceBatchSingleCrit", {
  fselect_args = formalArgs(fselect)
  fselect_args = fselect_args[fselect_args != "fselector"]
  fselect_args[fselect_args == "measures"] = "measure"

  instance_args = formalArgs(FSelectInstanceBatchSingleCrit$public_methods$initialize)
  instance_args = c(instance_args, "term_evals", "term_time")

  expect_set_equal(fselect_args, instance_args)
})

test_that("fselect interface is equal to FSelectInstanceBatchMultiCrit", {
  fselect_args = formalArgs(fselect)
  fselect_args = fselect_args[fselect_args %nin% c("fselector", "ties_method")]

  instance_args = formalArgs(FSelectInstanceBatchMultiCrit$public_methods$initialize)
  instance_args = c(instance_args, "term_evals", "term_time")

  expect_set_equal(fselect_args, instance_args)
})
