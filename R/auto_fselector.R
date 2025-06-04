#' @title Function for Automatic Feature Selection
#'
#' @inherit AutoFSelector description
#' @inheritSection AutoFSelector Resources
#' @inherit AutoFSelector details
#' @inheritSection AutoFSelector Nested Resampling
#'
#' @return [AutoFSelector].
#'
#' @template param_fselector
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_term_evals
#' @template param_term_time
#' @template param_terminator
#' @template param_store_fselect_instance
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#' @template param_ties_method
#' @template param_rush
#' @template param_id
#'
#' @export
#' @examples
#' afs = auto_fselector(
#'   fselector = fs("random_search"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' afs$train(tsk("pima"))
auto_fselector = function(
  fselector,
  learner,
  resampling,
  measure = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  store_fselect_instance = TRUE,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  ties_method = "least_features",
  rush = NULL,
  id = NULL
  ) {
  terminator = terminator %??% terminator_selection(term_evals, term_time)

  AutoFSelector$new(
    fselector = fselector,
    learner = learner,
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    store_fselect_instance = store_fselect_instance,
    store_benchmark_result = store_benchmark_result,
    store_models = store_models,
    check_values = check_values,
    callbacks = callbacks,
    ties_method = ties_method,
    rush = rush,
    id = id)
}
