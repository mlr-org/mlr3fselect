skip_if_not_installed("rush")
skip_if_no_redis()

test_that("FSelectorAsyncExhaustiveSearch works", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  fselector = fs("async_exhaustive_search")
  expect_class(fselector, "FSelectorAsync")

  instance = fsi_async(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("dummy"),
    terminator = trm("evals", n_evals = 15),
    rush = rush
  )

  expect_data_table(fselector$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, nrows = 15)
})
