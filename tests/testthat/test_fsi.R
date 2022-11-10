test_that("fsi function creates a FSelectInstanceSingleCrit", {
  instance = fsi(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2))
  expect_class(instance, "FSelectInstanceSingleCrit")
})

test_that("fsi function creates a FSelectInstanceMultiCrit", {
  instance = fsi(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 2))
  expect_class(instance, "FSelectInstanceMultiCrit")
})
