#' @title Function for Feature Selection
#'
#' @include FSelectInstanceBatchSingleCrit.R ArchiveBatchFSelect.R
#'
#' @description
#' Function to optimize the features of a [mlr3::Learner].
#' The function internally creates a [FSelectInstanceBatchSingleCrit] or [FSelectInstanceBatchMultiCrit] which describes the feature selection problem.
#' It executes the feature selection with the [FSelector] (`method`) and returns the result with the fselect instance (`$result`).
#' The [ArchiveBatchFSelect] (`$archive`) stores all evaluated hyperparameter configurations and performance scores.
#'
#' @details
#' The [mlr3::Task], [mlr3::Learner], [mlr3::Resampling], [mlr3::Measure] and [Terminator] are used to construct a [FSelectInstanceBatchSingleCrit].
#' If multiple performance [Measures][Measure] are supplied, a [FSelectInstanceBatchMultiCrit] is created.
#' The parameter `term_evals` and `term_time` are shortcuts to create a [Terminator].
#' If both parameters are passed, a [TerminatorCombo] is constructed.
#' For other [Terminators][Terminator], pass one with `terminator`.
#' If no termination criterion is needed, set `term_evals`, `term_time` and `terminator` to `NULL`.
#'
#' @inheritSection FSelectInstanceBatchSingleCrit Resources
#' @inheritSection ArchiveBatchFSelect Analysis
#'
#' @param measures ([mlr3::Measure] or list of [mlr3::Measure])\cr
#'   A single measure creates a [FSelectInstanceBatchSingleCrit] and multiple measures a [FSelectInstanceBatchMultiCrit].
#'   If `NULL`, default measure is used.
#'
#' @return [FSelectInstanceBatchSingleCrit] | [FSelectInstanceBatchMultiCrit]
#'
#' @template param_fselector
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_term_evals
#' @template param_term_time
#' @template param_terminator
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#' @template param_ties_method
#'
#' @export
#' @examples
#' # Feature selection on the Palmer Penguins data set
#' task = tsk("pima")
#' learner = lrn("classif.rpart")
#'
#' # Run feature selection
#' instance = fselect(
#'   fselector = fs("random_search"),
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp ("holdout"),
#'   measures = msr("classif.ce"),
#'   term_evals = 4)
#'
#' # Subset task to optimized feature set
#' task$select(instance$result_feature_set)
#'
#' # Train the learner with optimal feature set on the full data set
#' learner$train(task)
#'
#' # Inspect all evaluated configurations
#' as.data.table(instance$archive)
fselect = function(
  fselector,
  task,
  learner,
  resampling,
  measures = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  ties_method = "least_features"
  ) {
  assert_fselector(fselector)
  terminator = terminator %??% terminator_selection(term_evals, term_time)

  instance = if (!is.list(measures)) {
    FSelectInstanceBatchSingleCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measure = measures,
      terminator = terminator,
      store_benchmark_result = store_benchmark_result,
      store_models = store_models,
      check_values = check_values,
      callbacks = callbacks,
      ties_method = ties_method)
  } else {
    FSelectInstanceBatchMultiCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measures = measures,
      terminator = terminator,
      store_benchmark_result = store_benchmark_result,
      store_models = store_models,
      check_values = check_values,
      callbacks = callbacks)
  }

  fselector$optimize(instance)
  instance
}

terminator_selection = function(term_evals, term_time) {
  assert_int(term_evals, null.ok = TRUE)
  assert_int(term_time, null.ok = TRUE)

  if (is.null(term_evals) && is.null(term_time)) {
    trm("none")
  } else if (!is.null(term_evals) && !is.null(term_time)) {
    trm("combo", list(trm("evals", n_evals = term_evals), trm("run_time", secs = term_time)))
  } else if (!is.null(term_evals)) {
    trm("evals", n_evals = term_evals)
  } else if (!is.null(term_time)) {
    trm("run_time", secs = term_time)
  }
}
