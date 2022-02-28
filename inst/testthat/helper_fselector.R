test_fselector = function(.key, ..., term_evals = NULL, store_models = FALSE) {
  fselector = fs(.key, ...)
  expect_fselector(fselector)
  expect_man_exists(fselector$man)

  inst = fselect(
    method = .key,
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("dummy"),
    term_evals = term_evals,
    store_models = store_models,
    ...
  )

  # result checks
  archive = inst$archive
  expect_data_table(inst$result, nrows = 1)
  expect_names(names(inst$result), identical.to = c("x1", "x2", "x3", "x4", "features", "dummy"))
  expect_subset(inst$result$features[[1]], c("x1", "x2", "x3", "x4"))
  expect_data_table(inst$result_x_search_space, nrows = 1, ncols = 4, types = "logical")
  expect_names(names(inst$result_x_search_space), identical.to = c("x1", "x2", "x3", "x4"))
  expect_names(names(inst$result_y), identical.to = "dummy")

  list(fselector = fselector, inst = inst)
}

test_fselector_2D = function(.key, ..., term_evals = NULL, store_models = FALSE) {
  fselector = fs(.key, ...)
  expect_fselector(fselector)
  expect_man_exists(fselector$man)

  inst = fselect(
    method = .key,
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msrs(c("regr.rmse", "regr.mse")),
    term_evals = term_evals,
    store_models = store_models,
    ...
  )

  # result checks
  expect_names(names(inst$result), identical.to = c("x1", "x2", "x3", "x4", "features", "regr.rmse", "regr.mse"))
  expect_subset(inst$result$features[[1]], c("x1", "x2", "x3", "x4"))
  expect_data_table(inst$result_x_search_space, types = "logical")
  expect_names(names(inst$result_x_search_space), identical.to = c("x1", "x2", "x3", "x4"))
  expect_names(names(inst$result_y), identical.to = c("regr.rmse", "regr.mse"))

  list(fselector = fselector, inst = inst)
}
