test_that("FSelectorAsyncDesignPoints works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")
  instance = fsi_async(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("dummy"),
    terminator = trm("none"),
    store_benchmark_result = FALSE
  )

  design = data.table(
    x1 = c(TRUE, FALSE),
    x2 = c(TRUE, FALSE),
    x3 = c(FALSE, TRUE),
    x4 = c(FALSE, TRUE))

  fselector = fs("async_design_points", design = design)
  expect_data_table(fselector$optimize(instance), nrows = 1)

  expect_data_table(instance$archive$data, nrows = 2)
  expect_rush_reset(instance$rush, type = "kill")
})
