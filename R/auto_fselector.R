#' @title Function for Automatic Feature Selection
#'
#' @inherit AutoFSelector description
#' @inheritSection AutoFSelector Resources
#' @inherit AutoFSelector details
#' @inheritSection AutoFSelector Nested Resampling
#'
#' @param method (`character(1)` | [FSelector])\cr
#'  Key to retrieve fselector from [mlr_fselectors] dictionary or [FSelector] object.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the fselector.
#'
#' @return [AutoFSelector].
#'
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_terminator
#' @template param_store_fselect_instance
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#'
#' @export
#' @inherit AutoFSelector examples
auto_fselector = function(method, learner, resampling, measure = NULL, term_evals = NULL, term_time = NULL, terminator = NULL, store_fselect_instance = TRUE, store_benchmark_result = TRUE, store_models = FALSE, check_values = FALSE, ...) {
  fselector = if (is.character(method)) {
    assert_choice(method, mlr_fselectors$keys())
    fs(method, ...)
  } else {
    assert_fselector(method)
  }
  terminator = terminator %??% terminator_selection(term_evals, term_time)

  AutoFSelector$new(learner = learner, resampling = resampling, measure = measure, terminator = terminator, fselector = fselector, store_fselect_instance = store_fselect_instance, store_benchmark_result = store_benchmark_result, store_models = store_models, check_values = check_values)
}
