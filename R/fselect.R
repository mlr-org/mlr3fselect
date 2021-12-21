#' @title Function for Feature Selection
#'
#' @description
#' Function to optimize the feature set of a [mlr3::Learner].
#'
#' @param method (`character(1)`)\cr
#'  Key to retrieve fselector from [mlr_fselectors] dictionary.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the fselector.
#'
#' @return `FSelectInstanceSingleCrit` | `FSelectInstanceMultiCrit`
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_store_models
#'
#' @export
#' @examples
#' task = tsk("pima")
#'
#' instance = fselect(
#'   method = "random_search",
#'   task = task,
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp ("holdout"),
#'   measures = msr("classif.ce"),
#'   term_evals = 4)
#'
#' # subset task to optimized feature set
#' task$select(instance$result_feature_set)
fselect = function(method, task, learner, resampling, measures, term_evals = NULL, term_time = NULL,
  store_models = FALSE, ...) {
  assert_choice(method, mlr_fselectors$keys())
  fselector = fs(method, ...)
  terminator = terminator_selection(term_evals, term_time)

  instance = if (!is.list(measures)) {
    FSelectInstanceSingleCrit$new(task, learner, resampling, measures, terminator, store_models)
  } else {
    FSelectInstanceMultiCrit$new(task, learner, resampling, measures, terminator, store_models)
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
