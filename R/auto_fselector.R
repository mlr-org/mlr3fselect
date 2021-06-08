#' @title Syntactic Sugar for Automatic Feature Selection
#'
#' @description
#' Function to create an [AutoFSelector] object.
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
#' @return [AutoFSelector]
#'
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#'
#' @export
#' @examples
#' at = auto_fselector(
#'   method = "random_search",
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 50,
#'   batch_size = 10)
#'
#' at$train(tsk("pima"))
auto_fselector = function(method, learner, resampling, measure, term_evals = NULL, term_time = NULL, ...) {
  assert_choice(method, mlr_fselectors$keys())
  fselector = fs(method, ...)
  terminator = terminator_selection(term_evals, term_time)

  AutoFSelector$new(learner, resampling, measure, terminator, fselector)
}