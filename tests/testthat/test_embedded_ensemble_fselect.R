test_that("embedded efs works", {
  task = tsk("sonar")
  with_seed(42, {
    efsr = embedded_ensemble_fselect(
      task = task,
      learners = lrns(c("classif.rpart", "classif.featureless")),
      init_resampling = rsmp("subsampling", repeats = 5),
      measure = msr("classif.ce")
    )
  })

  expect_character(efsr$man)
  expect_data_table(efsr$result, nrows = 10)
  expect_list(efsr$result$features, any.missing = FALSE, len = 10)
  expect_numeric(efsr$result$n_features, len = 10)
  expect_numeric(efsr$result$classif.ce, len = 10)
  expect_benchmark_result(efsr$benchmark_result)
  expect_measure(efsr$measure)
  expect_equal(efsr$measure$id, "classif.ce")
  expect_true(efsr$measure$minimize) # classification error
  expect_equal(efsr$n_learners, 2)
  expect_equal(efsr$n_resamples, 5)

  # stability
  expect_number(efsr$stability(stability_measure = "jaccard", stability_args = list(impute.na = 0)))
  expect_error(efsr$stability(stability_args = list(20)), "have names")
  stability = efsr$stability(stability_measure = "jaccard", stability_args = list(impute.na = 0), global = FALSE)
  expect_numeric(stability, len = 2)
  expect_equal(names(stability), c("classif.rpart", "classif.featureless"))

  # pareto_front
  pf = efsr$pareto_front()
  expect_data_table(pf, nrows = 7)
  expect_equal(names(pf), c("n_features", "classif.ce"))
  pf_pred = efsr$pareto_front(type = "estimated")
  expect_data_table(pf_pred, nrows = max(efsr$result$n_features))
  expect_equal(names(pf_pred), c("n_features", "classif.ce"))

  # knee_points
  kps = efsr$knee_points()
  expect_data_table(kps, nrows = 1)
  expect_equal(names(kps), c("n_features", "classif.ce"))
  kpse = efsr$knee_points(type = "estimated")
  expect_data_table(kpse, nrows = 1)
  expect_true(kps$n_features != kpse$n_features)

  # data.table conversion
  tab = as.data.table(efsr)
  expect_equal(names(tab), c("learner_id", "resampling_iteration", "classif.ce",
                             "features", "n_features",
                             "task", "learner", "resampling"))

  # cannot change to use inner_measure
  expect_error(efsr$set_active_measure(which = "inner"), "No inner_measure was defined")
  # changing to "outer" leaves us with the same measure
  efsr$set_active_measure(which = "outer")
  expect_equal(efsr$measure$id, "classif.ce") # classification error

  # default feature ranking
  skip_if_not_installed("fastVoteR")
  feature_ranking = efsr$feature_ranking()
  expect_data_table(feature_ranking, nrows = length(task$feature_names))
  expect_equal(names(feature_ranking), c("feature", "score", "norm_score", "borda_score"))
})

test_that("combine embedded efs results", {
  task = tsk("sonar")
  with_seed(42, {
    efsr1 = embedded_ensemble_fselect(
      task = task,
      learners = lrns(c("classif.rpart", "classif.featureless")),
      init_resampling = rsmp("subsampling", repeats = 2),
      measure = msr("classif.ce")
    )
  })

  with_seed(43, {
    efsr2 = embedded_ensemble_fselect(
      task = task,
      learners = lrns(c("classif.rpart", "classif.featureless")),
      init_resampling = rsmp("subsampling", repeats = 3),
      measure = msr("classif.ce")
    )
  })

  comb1 = efsr1$clone(deep = TRUE)$combine(efsr2)
  comb2 = c(efsr1, efsr2)

  expect_class(comb1, "EnsembleFSResult")
  expect_class(comb2, "EnsembleFSResult")
  expect_data_table(comb1$result, nrows = 10L)
  expect_data_table(comb2$result, nrows = 10L)
  expect_equal(comb1$n_learners, 2L)
  expect_equal(comb2$n_learners, 2L)
  expect_equal(get_private(comb1)$.measure$id, "classif.ce")
  expect_equal(get_private(comb2)$.measure$id, "classif.ce")
  expect_null(get_private(comb1)$.inner_measure)
  expect_null(get_private(comb2)$.inner_measure)
  assert_benchmark_result(comb1$benchmark_result)
  assert_benchmark_result(comb2$benchmark_result)
  expect_equal(comb1$benchmark_result$n_resample_results, 4L)
  expect_equal(comb2$benchmark_result$n_resample_results, 4L)
  expect_equal(nrow(get_private(comb1$benchmark_result)$.data$data$fact), 10L)
  expect_equal(nrow(get_private(comb2$benchmark_result)$.data$data$fact), 10L)
})
