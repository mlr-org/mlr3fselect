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

test_that("fsi and FSelectInstanceSingleCrit are equal", {
    fsi_args = formalArgs(fsi)
    fsi_args[fsi_args == "measures"] = "measure"

    expect_equal(fsi_args, formalArgs(FSelectInstanceSingleCrit$public_methods$initialize))

    task = tsk("pima")
    learner = lrn("classif.rpart")
    resampling = rsmp ("holdout")
    measures = msr("classif.ce")
    terminator = trm("evals", n_evals = 2)
    store_benchmark_result = FALSE
    store_models = TRUE
    check_values = TRUE
    callbacks = clbk("mlr3fselect.backup")
    resampling$instantiate(task)

    instance_1 = FSelectInstanceSingleCrit$new(task, learner, resampling, measures, terminator, store_benchmark_result, store_models, check_values, callbacks)
    instance_2 = fsi(task, learner, resampling, measures, terminator, store_benchmark_result, store_models, check_values, callbacks)

    expect_equal(instance_1, instance_2)
})

test_that("fsi and FSelectInstanceMultiCrit are equal", {
    fsi_args = formalArgs(fsi)
    fsi_args = fsi_args[fsi_args != "ties_method"]

    expect_equal(fsi_args, formalArgs(FSelectInstanceMultiCrit$public_methods$initialize))

    task = tsk("pima")
    learner = lrn("classif.rpart")
    resampling = rsmp ("holdout")
    measures = msrs(c("classif.ce", "classif.acc"))
    terminator = trm("evals", n_evals = 2)
    store_benchmark_result = FALSE
    store_models = TRUE
    check_values = TRUE
    callbacks = clbk("mlr3fselect.backup")
    resampling$instantiate(task)

    instance_1 = FSelectInstanceMultiCrit$new(task, learner, resampling, measures, terminator, store_benchmark_result, store_models, check_values, callbacks)
    instance_2 = fsi(task, learner, resampling, measures, terminator, store_benchmark_result, store_models, check_values, callbacks)

    expect_equal(instance_1, instance_2)
})
