test_that("fsi_async function creates a FSelectInstanceAsyncSingleCrit", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2))
  expect_class(instance, "FSelectInstanceAsyncSingleCrit")
})

test_that("fsi_async function creates a FSelectInstanceAsyncMultiCrit", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  instance = fsi_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 2))
  expect_class(instance, "FSelectInstanceAsyncMultiCrit")
})

test_that("fsi_async interface is equal to FSelectInstanceAsyncSingleCrit", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  fsi_args = formalArgs(fsi_async)
  fsi_args[fsi_args == "measures"] = "measure"
  instance_args = formalArgs(FSelectInstanceAsyncSingleCrit$public_methods$initialize)

  expect_equal(fsi_args, instance_args)
})

test_that("fsi_async interface is equal to FSelectInstanceAsyncMultiCrit", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  fsi_args = formalArgs(fsi_async)
  fsi_args = fsi_args[fsi_args != "ties_method"]
  instance_args = formalArgs(FSelectInstanceAsyncMultiCrit$public_methods$initialize)

  expect_equal(fsi_args, instance_args)
})
