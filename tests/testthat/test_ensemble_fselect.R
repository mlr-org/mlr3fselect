test_that("ensemble feature selection works", {
  task = tsk("sonar")
  with_seed(42, {
    efsr = ensemble_fselect(
      fselector = fs("random_search"),
      task = task,
      learners = lrns(c("classif.rpart", "classif.featureless")),
      init_resampling = rsmp("subsampling", repeats = 2),
      inner_resampling = rsmp("cv", folds = 3),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 5)
    )
  })

  expect_character(efsr$man)
  expect_data_table(efsr$result, nrows = 4)
  expect_list(efsr$result$features, any.missing = FALSE, len = 4)
  expect_vector(efsr$result$n_features, size = 4)
  expect_vector(efsr$result$classif.ce, size = 4)
  expect_benchmark_result(efsr$benchmark_result)
  expect_equal(efsr$measure, "classif.ce")
  expect_equal(efsr$n_learners, 2)
  expect_equal(efsr$n_resamples, 2)

  # stability
  expect_number(efsr$stability(stability_measure = "jaccard"))
  expect_error(efsr$stability(stability_args = list(20)), "have names")
  stability = efsr$stability(stability_measure = "jaccard", global = FALSE)
  expect_numeric(stability, len = 2)
  expect_equal(names(stability), c("classif.rpart", "classif.featureless"))

  # default feature ranking
  feature_ranking = efsr$feature_ranking()
  expect_data_table(feature_ranking, nrows = length(task$feature_names))
  expect_equal(names(feature_ranking), c("feature", "score", "norm_score", "borda_score"))

  # pareto_front
  pf = efsr$pareto_front()
  expect_data_table(pf, nrows = 3)
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
  expect_equal(names(tab), c("resampling_iteration", "learner_id", "features", "n_features", "classif.ce", "task", "learner", "resampling"))
})

test_that("ensemble feature selection works without benchmark result", {
  task = tsk("sonar")
  with_seed(42, {
    efsr = ensemble_fselect(
      fselector = fs("random_search"),
      task = task,
      learners = lrns(c("classif.rpart", "classif.featureless")),
      init_resampling = rsmp("subsampling", repeats = 2),
      inner_resampling = rsmp("cv", folds = 3),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 3),
      store_benchmark_result = FALSE
    )
  })

  expect_character(efsr$man)
  expect_data_table(efsr$result, nrows = 4)
  expect_list(efsr$result$features, any.missing = FALSE, len = 4)
  expect_vector(efsr$result$n_features, size = 4)
  expect_vector(efsr$result$classif.ce, size = 4)
  expect_null(efsr$benchmark_result)
  expect_equal(efsr$measure, "classif.ce")
  expect_equal(efsr$n_learners, 2)
  expect_equal(efsr$n_resamples, 2)

  # stability
  expect_number(efsr$stability(stability_measure = "jaccard"))
  stability = efsr$stability(stability_measure = "jaccard", global = FALSE)
  expect_numeric(stability, len = 2)
  expect_equal(names(stability), c("classif.rpart", "classif.featureless"))

  # default feature ranking
  feature_ranking = efsr$feature_ranking()
  expect_data_table(feature_ranking, nrows = length(task$feature_names))
  expect_equal(names(feature_ranking), c("feature", "score", "norm_score", "borda_score"))

  # pareto_front
  pf = efsr$pareto_front()
  expect_data_table(pf, nrows = 3)
  expect_equal(names(pf), c("n_features", "classif.ce"))

  # knee_points
  kps = efsr$knee_points()
  expect_data_table(kps, nrows = 1)
  expect_equal(names(kps), c("n_features", "classif.ce"))

  # data.table conversion
  tab = as.data.table(efsr)
  expect_equal(names(tab), c("resampling_iteration", "learner_id", "features", "n_features", "classif.ce"))
})

test_that("ensemble feature selection works with rfe", {
  task = tsk("sonar")
  with_seed(42, {
    efsr = ensemble_fselect(
      fselector = fs("rfe", n_features = 2, feature_fraction = 0.8),
      task = task,
      learners = lrns(c("classif.rpart", "classif.featureless")),
      init_resampling = rsmp("subsampling", repeats = 2),
      inner_resampling = rsmp("cv", folds = 3),
      measure = msr("classif.ce"),
      terminator = trm("none")
    )
  })

  expect_character(efsr$man)
  expect_data_table(efsr$result, nrows = 4)
  expect_list(efsr$result$features, any.missing = FALSE, len = 4)
  expect_vector(efsr$result$n_features, size = 4)
  expect_vector(efsr$result$classif.ce, size = 4)
  expect_list(efsr$result$importance, any.missing = FALSE, len = 4)
  expect_benchmark_result(efsr$benchmark_result)
  expect_equal(efsr$measure, "classif.ce")
  expect_equal(efsr$n_learners, 2)
  expect_equal(efsr$n_resamples, 2)

  # stability
  expect_number(efsr$stability(stability_measure = "jaccard"))
  stability = efsr$stability(stability_measure = "jaccard", global = FALSE)
  expect_numeric(stability, len = 2)
  expect_equal(names(stability), c("classif.rpart", "classif.featureless"))

  # default feature ranking
  feature_ranking = efsr$feature_ranking()
  expect_data_table(feature_ranking, nrows = length(task$feature_names))
  expect_equal(names(feature_ranking), c("feature", "score", "norm_score", "borda_score"))

  # pareto_front
  pf = efsr$pareto_front()
  expect_data_table(pf, nrows = 4)
  expect_equal(names(pf), c("n_features", "classif.ce"))
  pf_pred = efsr$pareto_front(type = "estimated")
  expect_data_table(pf_pred, nrows = max(efsr$result$n_features))
  expect_equal(names(pf_pred), c("n_features", "classif.ce"))

  # knee_points
  kps = efsr$knee_points(type = "estimated")
  expect_data_table(kps, nrows = 1)
  expect_equal(names(kps), c("n_features", "classif.ce"))

  # data.table conversion
  tab = as.data.table(efsr)
  expect_names(names(tab), identical.to = c("resampling_iteration", "learner_id", "features", "n_features", "classif.ce", "importance", "task", "learner", "resampling"))
})

test_that("EnsembleFSResult initialization", {
  result = data.table(a = 1, b = 3)
  expect_error(EnsembleFSResult$new(result = result, features = LETTERS, measure_id = "a"), "missing elements")

  result = data.table(
    resampling_iteration = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    learner_id = rep(c("classif.xgboost", "classif.rpart", "classif.ranger"), 3),
    n_features = c(2, 4, 4, 1, 5, 4, 1, 2, 4),
    features = list(
      c("V3", "V20"),
      c("V3", "V5", "V19", "V15"),
      c("V11", "V7", "V6", "V8"),
      c("V11"),
      c("V17", "V2", "V12", "V9", "V1"),
      c("V11", "V18", "V9", "V2"),
      c("V2"),
      c("V4", "V12"),
      c("V6", "V15", "V19", "V7")),
    classif.ce = c(0.13, 0.24, 0.16, 0.11, 0.25, 0.18, 0.15, 0.1, 0.16)
  )

  # a feature set includes "V20" which is not included in input "features"
  expect_error(EnsembleFSResult$new(result = result, features = paste0("V", 1:19), measure_id = "classif.ce"), "Must be a subset of")

  # works without benchmark result object
  efsr = EnsembleFSResult$new(result = result, features = paste0("V", 1:20), measure_id = "classif.ce")
  expect_class(efsr, "EnsembleFSResult")
  expect_equal(efsr$n_learners, 3)
  expect_equal(efsr$n_resamples, 3)
  tab = as.data.table(efsr)
  expect_data_table(tab)
  expect_equal(names(tab), c("resampling_iteration", "learner_id", "n_features",
                             "features", "classif.ce"))
})

test_that("different callbacks can be set", {
  callback_test = callback_batch_fselect("mlr3fselect.test",
    on_eval_before_archive = function(callback, context) {
      context$aggregated_performance[, callback_active := context$instance$objective$learner$id == "classif.rpart"]
    }
  )

  efsr = ensemble_fselect(
    # 4-5 evaluations on sonar
    fselector = fs("rfe", n_features = 25, feature_fraction = 0.8),
    task = tsk("sonar"),
    learners = lrns(c("classif.rpart", "classif.featureless")),
    init_resampling = rsmp("subsampling", repeats = 2),
    inner_resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("none"),
    callbacks = list(list(callback_test), list())
  )

  expect_true(all(efsr$benchmark_result$score()$learner[[1]]$fselect_instance$archive$data$callback_active))
  expect_null(efsr$benchmark_result$score()$learner[[2]]$fselect_instance$archive$data$callback_active)
})
