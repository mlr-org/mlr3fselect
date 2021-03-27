test_that("FSelectorShadowVariableSearch", {
  test_fselector("shadow_variable_search", term_evals = 26, store_models = TRUE)

  # forward selection works
  instance = TEST_MAKE_INST_1D(terminator = trm("none"))
  task = instance$objective$task$clone()
  domain = instance$objective$domain$clone()
  fselector = fs("shadow_variable_search")
  fselector$optimize(instance)

  expect_best_features(instance$archive$best(batch = 1)[, 1:8], "x1")
  expect_best_features(instance$archive$best(batch = 2)[, 1:8], c("x1", "x2"))
  expect_best_features(instance$archive$best(batch = 3)[, 1:8], c("x1", "x2", "x3"))
  expect_best_features(instance$archive$best(batch = 4)[, 1:8], c("x1", "x2", "x3", "x4"))


  # first selected feature is a shadow variable
  score_design = data.table(score = 1, features = "permuted__x1")
  instance = TEST_MAKE_INST_1D(measure = msr("dummy", score_design = score_design), terminator = trm("none"))
  fselector = fs("shadow_variable_search")
  expect_error(fselector$optimize(instance),
    regexp = "The first selected feature is a shadow variable.")


  # second selected feature is a shadow variable
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


  # search is terminated by terminator
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
