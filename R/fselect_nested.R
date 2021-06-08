#' @title Function for Nested Resampling
#'
#' @description
#' Function to conduct nested resampling.
#'
#' @param method (`character(1)`)\cr
#'  Key to retrieve fselector from [mlr_fselectors] dictionary.
#' @param inner_resampling ([mlr3::Resampling])\cr
#'  Resampling used for the inner loop.
#' @param outer_resampling [mlr3::Resampling])\cr
#'  Resampling used for the outer loop.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the fselector.
#'
#' @return [mlr3::ResampleResult]
#'
#' @template param_task
#' @template param_learner
#' @template param_measure
#'
#' @export
#' @examples
#' rr = fselect_nested(
#'   method = "random_search",
#'   task = tsk("pima"),
#'   learner = lrn("classif.rpart"),
#'   inner_resampling = rsmp ("holdout"),
#'   outer_resampling = rsmp("cv", folds = 2),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' # performance scores estimated on the outer resampling
#' rr$score()
#'
#' # unbiased performance of the final model trained on the full data set
#' rr$aggregate()
fselect_nested = function(method, task, learner, inner_resampling, outer_resampling, measure, term_evals = NULL,
  term_time = NULL, ...) {
  assert_task(task)
  assert_resampling(inner_resampling)
  assert_resampling(outer_resampling)

  afs = auto_fselector(method, learner, inner_resampling, measure, term_evals, term_time, ...)
  resample(task, afs, outer_resampling, store_models = TRUE)
}