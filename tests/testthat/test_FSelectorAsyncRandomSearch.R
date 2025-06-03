test_that("FSelectorAsyncRandomSearch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  fselector = fs("async_random_search")
  expect_class(fselector, "FSelectorAsync")

  rush::rush_plan(n_workers = 2)
  instance = fsi_async(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("dummy"),
    terminator = trm("evals", n_evals = 5L)
  )

  expect_data_table(fselector$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 5)

  expect_rush_reset(instance$rush)
})