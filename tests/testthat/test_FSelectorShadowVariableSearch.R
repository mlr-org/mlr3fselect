test_that("default parameters work", {
  z = test_fselector("shadow_variable_search", store_models = TRUE)

  expect_best_features(z$inst$archive$best(batch = 1)[, 1:8], "x1")
  expect_best_features(z$inst$archive$best(batch = 2)[, 1:8], c("x1", "x2"))
  expect_best_features(z$inst$archive$best(batch = 3)[, 1:8], c("x1", "x2", "x3"))
  expect_best_features(z$inst$archive$best(batch = 4)[, 1:8], c("x1", "x2", "x3", "x4"))
})

test_that("task is permuted", {
  instance = TEST_MAKE_INST_1D(terminator = trm("none"))
  task = instance$objective$task$clone()
  fselector = fs("shadow_variable_search")
  fselector$optimize(instance)

  task_permuted = instance$archive$benchmark_result$tasks$task[[1]]
  expect_set_equal(task_permuted$feature_names, c("x1", "x2", "x3", "x4", "permuted__x1", "permuted__x2", "permuted__x3", "permuted__x4"))
  expect_equal(task_permuted$data()[, 1:5], task$data())
  expect_false(isTRUE(all.equal(task_permuted$data()[, 6:9], task$data()[, 2:5])))
  expect_set_equal(task_permuted$data()[[1]], task$data()[[1]], ordered = TRUE)
  expect_set_equal(task_permuted$data()[[6]], task$data()[[2]])
  expect_set_equal(task_permuted$data()[[7]], task$data()[[3]])
  expect_set_equal(task_permuted$data()[[8]], task$data()[[4]])
  expect_set_equal(task_permuted$data()[[9]], task$data()[[5]])
})

test_that("first selected feature is a shadow variable works", {
  score_design = data.table(score = 1, features = "permuted__x1")
  instance = TEST_MAKE_INST_1D(measure = msr("dummy", score_design = score_design), terminator = trm("none"))
  fselector = fs("shadow_variable_search")
  expect_error(fselector$optimize(instance), regexp = "The first selected feature is a shadow variable.")
})

test_that("second selected feature is a shadow variable works", {
  score_design = data.table(score = c(1, 2), features = list("x1", c("x1", "permuted__x1")))
  instance = TEST_MAKE_INST_1D(measure = msr("dummy", score_design = score_design), terminator = trm("none"))
  task = instance$objective$task$clone()
  domain = instance$objective$domain$clone()
  fselector = fs("shadow_variable_search")
  fselector$optimize(instance)

  # two batch are evaluated but second batch is removed because the best feature subsets contains a shadow variable
  expect_equal(instance$archive$n_batch, 1)
  # expect that the best result is the one without shadow variable
  expect_equal(instance$result$features, list("x1"))
  expect_equal(instance$result_y, c("dummy" = 1))
  # check that domain and search space are restored
  expect_equal(instance$search_space, domain)
  expect_equal(instance$objective$domain, domain)
  # check that task is restored
  expect_equal(instance$objective$task, task)
})

test_that("search is terminated by terminator works", {
  instance = TEST_MAKE_INST_1D(terminator = trm("evals", n_evals = 15))
  task = instance$objective$task$clone()
  domain = instance$objective$domain$clone()
  fselector = fs("shadow_variable_search")
  fselector$optimize(instance)

  # check that last batch is not removed because the best feature subset contains no shadow variable
  expect_equal(instance$archive$n_batch, 2)
  expect_equal(instance$result$features, list(c("x1", "x2")))
  expect_equal(instance$result_y, c("dummy" = 2))
  # check that domain and search space are restored
  expect_equal(instance$search_space, domain)
  expect_equal(instance$objective$domain, domain)
  # check that task is restored
  expect_equal(instance$objective$task, task)
})
