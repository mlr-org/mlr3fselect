#' @title Ensemble Feature Selection
#'
#' @include CallbackBatchFSelect.R
#'
#' @description
#' Ensemble feature selection using multiple learners.
#' The ensemble feature selection method is designed to identify the most informative features from a given dataset by leveraging multiple machine learning models and resampling techniques.
#'
#' @details
#' The method begins by applying an initial resampling technique specified by the user, to create **multiple subsamples** from the original dataset.
#' This resampling process helps in generating diverse subsets of data for robust feature selection.
#'
#' For each subsample generated in the previous step, the method performs **wrapped-based feature selection** ([auto_fselector]) using each provided learner, the given inner resampling method, performance measure and optimization algorithm.
#' This process generates the best feature subset for each combination of subsample and learner.
#' Results are stored in an [EnsembleFSResult].
#'
#' @param learners (list of [mlr3::Learner])\cr
#'  The learners to be used for feature selection.
#' @param init_resampling ([mlr3::Resampling])\cr
#'  The initial resampling strategy of the data, from which each train set
#'  will be passed on to the learners.
#'  Can only be [mlr_resamplings_subsampling] or [mlr_resamplings_bootstrap].
#' @param inner_resampling ([mlr3::Resampling])\cr
#'  The inner resampling strategy used by the [FSelector].
#' @param store_benchmark_result (`logical(1)`)\cr
#'  Whether to store the benchmark result in [EnsembleFSResult] or not.
#' @param store_models (`logical(1)`)\cr
#'  Whether to store models in [auto_fselector] or not.
#' @param callbacks (list of lists of [CallbackBatchFSelect])\cr
#'  Callbacks to be used for each learner.
#'  The lists must have the same length as the number of learners.
#'
#' @template param_fselector
#' @template param_task
#' @template param_measure
#' @template param_terminator
#'
#' @returns an [EnsembleFSResult] object.
#'
#' @source
#' `r format_bib("saeys2008", "abeel2010", "pes2020")`
#' @export
#' @examples
#' \donttest{
#'   ensemble_fselect(
#'     fselector = fs("random_search"),
#'     task = tsk("sonar"),
#'     learners = lrns(c("classif.rpart", "classif.featureless")),
#'     init_resampling = rsmp("subsampling", repeats = 2),
#'     inner_resampling = rsmp("cv", folds = 3),
#'     measure = msr("classif.ce"),
#'     terminator = trm("evals", n_evals = 10)
#'   )
#' }
ensemble_fselect = function(
  fselector,
  task,
  learners,
  init_resampling,
  inner_resampling,
  measure,
  terminator,
  callbacks = NULL,
  store_benchmark_result = TRUE,
  store_models = TRUE
  ) {
  assert_task(task)
  assert_learners(as_learners(learners), task = task)
  assert_resampling(init_resampling)
  assert_choice(class(init_resampling)[1], choices = c("ResamplingBootstrap", "ResamplingSubsampling"))
  assert_list(callbacks, types = "list", len = length(learners), null.ok = TRUE)
  assert_flag(store_benchmark_result)

  # create auto_fselector for each learner
  afss = imap(unname(learners), function(learner, i) {
    auto_fselector(
      fselector = fselector,
      learner = learner,
      resampling = inner_resampling,
      measure = measure,
      terminator = terminator,
      store_models = store_models,
      callbacks = callbacks[[i]]
    )
  })

  init_resampling$instantiate(task)
  grid = map_dtr(seq(init_resampling$iters), function(i) {

    # create task and resampling for each outer iteration
    task_subset = task$clone()$filter(init_resampling$train_set(i))
    resampling = rsmp("insample")$instantiate(task_subset)

    data.table(
      resampling_iteration = i,
      learner_id = map_chr(learners, "id"),
      learner = afss,
      task = list(task_subset),
      resampling = list(resampling)
    )
  })

  design = grid[, c("learner", "task", "resampling"), with = FALSE]

  bmr = benchmark(design, store_models = TRUE)

  afss = bmr$score()$learner

  # extract features
  features = map(afss, function(afs) {
    afs$fselect_result$features[[1]]
  })

  # extract n_features
  n_features = map_int(afss, function(afs) {
    afs$fselect_result$n_features[[1]]
  })

  # extract scores
  scores = map_dbl(afss, function(afs) {
    afs$fselect_instance$archive$best()[, measure$id, with = FALSE][[1]]
  })

  set(grid, j = "features", value = features)
  set(grid, j = "n_features", value = n_features)
  set(grid, j = measure$id, value = scores)

  # extract importance scores if RFE optimization was used
  if (class(fselector)[1] == "FSelectorBatchRFE") {
    imp_scores = map(afss, function(afs) {
      afs$fselect_result$importance[[1]]
    })
    set(grid, j = "importance", value = imp_scores)
  }

  set(grid, j = "learner", value = NULL)
  set(grid, j = "task", value = NULL)
  set(grid, j = "resampling", value = NULL)
  EnsembleFSResult$new(
    result = grid,
    features = task$feature_names,
    benchmark_result = if (store_benchmark_result) bmr
  )
}
