test_that("efs works", {
  task = tsk("sonar")
  with_seed(42, {
    efsr = ensemble_fselect(
      fselector = fs("random_search"),
      task = task,
      learners = lrns(c("classif.rpart", "classif.featureless")),
      init_resampling = rsmp("subsampling", repeats = 2),
      inner_resampling = rsmp("cv", folds = 3),
      inner_measure = msr("classif.ce"),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 5)
    )
  })

  expect_character(efsr$man)
  expect_data_table(efsr$result, nrows = 4)
  expect_list(efsr$result$features, any.missing = FALSE, len = 4)
  expect_numeric(efsr$result$n_features, len = 4)
  expect_numeric(efsr$result$classif.ce, len = 4)
  expect_numeric(efsr$result$classif.ce_inner, len = 4)
  expect_benchmark_result(efsr$benchmark_result)
  expect_measure(efsr$measure)
  expect_equal(efsr$measure$id, "classif.ce")
  expect_true(efsr$measure$minimize) # classification error
  expect_equal(efsr$n_learners, 2)
  expect_equal(efsr$n_resamples, 2)

  # stability
  expect_number(efsr$stability(stability_measure = "jaccard"))
  expect_error(efsr$stability(stability_args = list(20)), "have names")
  stability = efsr$stability(stability_measure = "jaccard", global = FALSE)
  expect_numeric(stability, len = 2)
  expect_equal(names(stability), c("classif.rpart.fselector", "classif.featureless.fselector"))

  # pareto_front
  pf = efsr$pareto_front()
  expect_data_table(pf, nrows = 2)
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
                             "features", "n_features", "classif.ce_inner",
                             "task", "learner", "resampling"))
  # scores on train and test sets are different (even though same measure used)
  assert_true(all(tab$classif.ce != tab$classif.ce_inner))

  # change to use inner measure
  expect_error(efsr$set_active_measure(which = "XYZ"), regexp = "Must be element")
  efsr$set_active_measure(which = "inner")
  expect_measure(efsr$measure)
  expect_equal(efsr$measure$id, "classif.ce") # classification error also used for inner measure
  pf_inner = efsr$pareto_front()
  expect_data_table(pf_inner, nrows = 3) # pareto front has changed
  expect_equal(names(pf_inner), c("n_features", "classif.ce_inner"))
  kps_inner = efsr$knee_points()
  expect_data_table(kps_inner, nrows = 1)
  # inner id to distinguish from outer measure
  expect_equal(names(kps_inner), c("n_features", "classif.ce_inner"))
  # change to use outer measure again
  efsr$set_active_measure(which = "outer")
  pf_outer = efsr$pareto_front()
  expect_equal(pf_outer, pf) # same measure, same pareto front

  # default feature ranking
  skip_if_not_installed("fastVoteR")
  feature_ranking = efsr$feature_ranking()
  expect_data_table(feature_ranking, nrows = length(task$feature_names))
  expect_equal(names(feature_ranking), c("feature", "score", "norm_score", "borda_score"))
})

test_that("efs works with rfe", {
  task = tsk("sonar")
  with_seed(42, {
    efsr = ensemble_fselect(
      fselector = fs("rfe", subset_sizes = c(60, 20, 10, 5)),
      task = task,
      learners = lrns(c("classif.rpart", "classif.featureless")),
      init_resampling = rsmp("subsampling", repeats = 2),
      inner_resampling = rsmp("cv", folds = 3),
      inner_measure = msr("classif.ce"),
      measure = msr("classif.acc"),
      terminator = trm("none")
    )
  })

  expect_character(efsr$man)
  expect_data_table(efsr$result, nrows = 4)
  expect_list(efsr$result$features, any.missing = FALSE, len = 4)
  expect_numeric(efsr$result$n_features, len = 4)
  expect_numeric(efsr$result$classif.ce_inner, len = 4)
  expect_numeric(efsr$result$classif.acc, len = 4)
  expect_list(efsr$result$importance, any.missing = FALSE, len = 4)
  expect_benchmark_result(efsr$benchmark_result)
  expect_measure(efsr$measure)
  expect_equal(efsr$measure$id, "classif.acc")
  expect_false(efsr$measure$minimize) # accuracy
  expect_equal(efsr$n_learners, 2)
  expect_equal(efsr$n_resamples, 2)

  # change active measure
  efsr$set_active_measure(which = "inner")
  expect_measure(efsr$measure)
  expect_equal(efsr$measure$id, "classif.ce") # no `_inner` end-fix here
  expect_true(efsr$measure$minimize) # classification error

  # stability
  expect_number(efsr$stability(stability_measure = "jaccard"))
  stability = efsr$stability(stability_measure = "jaccard", global = FALSE)
  expect_numeric(stability, len = 2)
  expect_equal(names(stability), c("classif.rpart.fselector", "classif.featureless.fselector"))

  # pareto_front
  pf = efsr$pareto_front()
  expect_data_table(pf, nrows = 4)
  expect_equal(names(pf), c("n_features", "classif.ce_inner"))
  pf_pred = efsr$pareto_front(type = "estimated")
  expect_data_table(pf_pred, nrows = max(efsr$result$n_features))
  expect_equal(names(pf_pred), c("n_features", "classif.ce_inner"))

  # knee_points
  kps = efsr$knee_points(type = "estimated")
  expect_data_table(kps, nrows = 1)
  expect_equal(names(kps), c("n_features", "classif.ce_inner"))

  # change measure back to "outer"
  efsr$set_active_measure(which = "outer")
  pf_outer = efsr$pareto_front() # pareto front has used the accuracy measure
  expect_equal(names(pf_outer), c("n_features", "classif.acc"))

  # data.table conversion
  tab = as.data.table(efsr)
  expect_equal(names(tab), c("learner_id", "resampling_iteration", "classif.acc",
                             "features", "n_features", "classif.ce_inner",
                             "importance", "task", "learner", "resampling"))

  # default feature ranking
  skip_if_not_installed("fastVoteR")
  feature_ranking = efsr$feature_ranking()
  expect_data_table(feature_ranking, nrows = length(task$feature_names))
  expect_equal(names(feature_ranking), c("feature", "score", "norm_score", "borda_score"))
})

test_that("EnsembleFSResult initialization", {
  result = data.table(a = 1, b = 3)
  # `result` doesn't have mandatory columns
  expect_error(EnsembleFSResult$new(result = result, features = LETTERS,
                                    measure = msr("classif.ce")), "is missing")

  errors = c(0.13, 0.24, 0.16, 0.11, 0.25, 0.18, 0.15, 0.1, 0.16)
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
    classif.ce = errors,
    classif.acc_inner = 1 - errors # inner measure has the `_inner` end-fix
  )

  # a feature set includes "V20" which is not included in input "features"
  expect_error(EnsembleFSResult$new(result = result, features = paste0("V", 1:19), measure = msr("classif.ce")), "Must be a subset of")
  # `inner_measure` is not a `Measure`
  expect_error(EnsembleFSResult$new(result = result, features = paste0("V", 1:20),
                                    measure = msr("classif.ce"), inner_measure = "measure"))
  # `inner_measure` id is not a column name in the `result`
  expect_error(EnsembleFSResult$new(result = result, features = paste0("V", 1:20),
                                    measure = msr("classif.ce"), inner_measure = msr("classif.ce")))
  # both `inner_measure` and `measure` ids are missing from the `result`'s column names
  expect_error(EnsembleFSResult$new(result = result, features = paste0("V", 1:20),
                                    measure = msr("classif.acc"), inner_measure = msr("classif.ce")))

  # works without benchmark result object
  efsr = EnsembleFSResult$new(result = result, features = paste0("V", 1:20),
                              measure = msr("classif.ce"), inner_measure = msr("classif.acc"))
  expect_class(efsr, "EnsembleFSResult")
  expect_equal(efsr$n_learners, 3)
  expect_equal(efsr$n_resamples, 3)
  expect_equal(efsr$.__enclos_env__$private$.active_measure, "outer")
  expect_measure(efsr$measure)
  expect_equal(efsr$measure$id, "classif.ce")
  expect_true(efsr$measure$minimize)
  tab = as.data.table(efsr)
  expect_data_table(tab)
  expect_equal(names(tab), c("resampling_iteration", "learner_id", "n_features",
                             "features", "classif.ce", "classif.acc_inner"))
  # change active measure
  efsr$set_active_measure(which = "inner")
  expect_equal(efsr$.__enclos_env__$private$.active_measure, "inner")
  expect_measure(efsr$measure)
  expect_equal(efsr$measure$id, "classif.acc")
  expect_false(efsr$measure$minimize)
})

test_that("different callbacks can be set", {
  callback_test = callback_batch_fselect("mlr3fselect.test",
    on_eval_before_archive = function(callback, context) {
      context$aggregated_performance[, callback_active := context$instance$objective$learner$id == "classif.rpart"]
    }
  )

  efsr = ensemble_fselect(
    fselector = fs("rfe", subset_sizes = c(60, 20, 10, 5)),
    task = tsk("sonar"),
    learners = lrns(c("classif.rpart", "classif.featureless")),
    init_resampling = rsmp("subsampling", repeats = 2),
    inner_resampling = rsmp("cv", folds = 3),
    inner_measure = msr("classif.ce"),
    measure = msr("classif.acc"),
    terminator = trm("none"),
    callbacks = list("classif.rpart" = callback_test)
  )

  expect_true(all(efsr$benchmark_result$score()$learner[[1]]$fselect_instance$archive$data$callback_active))
  expect_null(efsr$benchmark_result$score()$learner[[3]]$fselect_instance$archive$data$callback_active)
})
