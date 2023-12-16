#' @title Function for Nested Resampling
#'
#' @description
#' Function to conduct nested resampling.
#'
#' @param inner_resampling ([mlr3::Resampling])\cr
#'  Resampling used for the inner loop.
#' @param outer_resampling [mlr3::Resampling])\cr
#'  Resampling used for the outer loop.
#'
#' @return [mlr3::ResampleResult]
#'
#' @template param_fselector
#' @template param_task
#' @template param_learner
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
#'
#' @export
#' @examples
#' # Nested resampling on Palmer Penguins data set
#' rr = fselect_nested(
#'   fselector = fs("random_search"),
#'   task = tsk("penguins"),
#'   learner = lrn("classif.rpart"),
#'   inner_resampling = rsmp ("holdout"),
#'   outer_resampling = rsmp("cv", folds = 2),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' # Performance scores estimated on the outer resampling
#' rr$score()
#'
#' # Unbiased performance of the final model trained on the full data set
#' rr$aggregate()
fselect_nested = function(
  fselector,
  task,
  learner,
  inner_resampling,
  outer_resampling,
  measure = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  store_fselect_instance = TRUE,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = list(),
  ties_method = "least_features"
  ) {
  assert_task(task)
  assert_resampling(inner_resampling)
  assert_resampling(outer_resampling)
  terminator = terminator %??% terminator_selection(term_evals, term_time)

  afs = auto_fselector(
    learner = learner,
    resampling = inner_resampling,
    measure = measure,
    terminator = terminator,
    fselector = fselector,
    store_fselect_instance = store_fselect_instance,
    store_benchmark_result = store_benchmark_result,
    store_models = store_models,
    check_values = check_values,
    callbacks = callbacks,
    ties_method = ties_method)

  resample(task, afs, outer_resampling, store_models = TRUE)
}
