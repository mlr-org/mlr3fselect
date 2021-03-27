test_fselector = function(.key, ..., term_evals = 2L, real_evals = term_evals, store_models = TRUE) {

  inst = FSelectInstanceSingleCrit$new(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measure = msr("dummy"),
    terminator = trm("evals", n_evals = term_evals),
    store_models)

  fselector = fs(.key, ...)
  expect_fselector(fselector)
  expect_data_table(fselector$optimize(inst), nrows = 1, ncols = 6, any.missing = FALSE)
  archive = inst$archive

  # Archive checks
  expect_data_table(archive$data, nrows = real_evals)
  expect_equal(inst$archive$n_evals, real_evals)

  # Result checks
  expect_data_table(inst$result, nrows = 1, ncols = 6)
  expect_named(inst$result, c(
    "x1",
    "x2",
    "x3",
    "x4",
    "features",
    "dummy"))
  expect_character(inst$result$features[[1]])
  expect_data_table(inst$result_x_search_space, nrows = 1, ncols = 4,
    types = "logical")
  expect_named(inst$result_x_search_space, c(
    "x1",
    "x2",
    "x3",
    "x4"))
  expect_named(inst$result_y, "dummy")

  list(fselector = fselector, inst = inst)
}

test_fselector_2D = function(.key, ..., term_evals = 2L, real_evals = term_evals, store_models = FALSE) {

  inst = FSelectInstanceMultiCrit$new(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msrs(c("regr.rmse", "regr.mse")),
    terminator = trm("evals", n_evals = term_evals),
    store_models)

  fselector = fs(.key, ...)
  expect_fselector(fselector)
  expect_data_table(fselector$optimize(inst), ncols = 7, any.missing = FALSE)
  archive = inst$archive

  # Archive checks
  expect_data_table(archive$data, nrows = real_evals)
  expect_equal(inst$archive$n_evals, real_evals)

  # Result checks
  expect_data_table(inst$result, ncols = 7)
  expect_named(inst$result, c(
    "x1",
    "x2",
    "x3",
    "x4",
    "features",
    "regr.rmse",
    "regr.mse"))
  expect_character(inst$result$features[[1]])
  expect_data_table(inst$result_x_search_space, ncols = 4, types = "logical")
  expect_named(inst$result_x_search_space, c(
    "x1",
    "x2",
    "x3",
    "x4"))
  expect_named(inst$result_y, c("regr.rmse", "regr.mse"))

  list(fselector = fselector, inst = inst)
}