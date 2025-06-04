test_that("FSelectorAsyncExhaustiveSearch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  fselector = fs("async_exhaustive_search")
  expect_class(fselector, "FSelectorAsync")

  on.exit(mirai::daemons(0))
  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")
  instance = fsi_async(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("dummy"),
    terminator = trm("evals", n_evals = 15)
  )

  expect_data_table(fselector$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, nrows = 15)

  expect_rush_reset(instance$rush, type = "kill")
})
