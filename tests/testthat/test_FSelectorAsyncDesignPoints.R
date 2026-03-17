skip_if_not_installed("rush")
skip_if_no_redis()

test_that("FSelectorAsyncDesignPoints works", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = fsi_async(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("dummy"),
    terminator = trm("evals", n_evals = 2),
    store_benchmark_result = FALSE,
    rush = rush
  )

  design = data.table(
    x1 = c(TRUE, FALSE),
    x2 = c(TRUE, FALSE),
    x3 = c(FALSE, TRUE),
    x4 = c(FALSE, TRUE))

  fselector = fs("async_design_points", design = design)
  expect_data_table(fselector$optimize(instance), nrows = 1)

  expect_data_table(instance$archive$data, nrows = 2)
})
