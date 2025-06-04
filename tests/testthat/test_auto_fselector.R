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

# Async ------------------------------------------------------------------------

test_that("async auto fselector works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  on.exit(mirai::daemons(0))
  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  afs = auto_fselector(
    fselector = fs("async_random_search"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3)
  )

  expect_class(afs, "AutoFSelector")
  afs$train(tsk("pima"))

  expect_class(afs$fselect_instance, "FSelectInstanceAsyncSingleCrit")
  expect_rush_reset(afs$fselect_instance$rush, type = "kill")
})

test_that("async auto fselector works with rush controller", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  on.exit(mirai::daemons(0))
  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")
  rush = rush::rsh(network_id = "fselect_network")

  afs = auto_fselector(
    fselector = fs("async_random_search"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    rush = rush
  )

  expect_class(afs, "AutoFSelector")
  expect_class(afs$instance_args$rush, "Rush")
  afs$train(tsk("pima"))

  expect_class(afs$fselect_instance, "FSelectInstanceAsyncSingleCrit")
  expect_rush_reset(afs$fselect_instance$rush, type = "kill")
})
