#' @title Extract Inner Feature Selection Results
#'
#' @description
#' Extract inner feature selection results of nested resampling.
#' Implemented for [mlr3::ResampleResult] and [mlr3::BenchmarkResult].
#'
#' @details
#' The function iterates over the [AutoFSelector] objects and binds the feature selection results to a [data.table::data.table()].
#' [AutoFSelector] must be initialized with `store_fselect_instance = TRUE` and `resample()` or `benchmark()` must be called with `store_models = TRUE`.
#' Optionally, the instance can be added for each iteration.
#'
#' @section Data structure:
#'
#' The returned data table has the following columns:
#'
#' * `experiment` (integer(1))\cr
#'   Index, giving the according row number in the original benchmark grid.
#' * `iteration` (integer(1))\cr
#'   Iteration of the outer resampling.
#' * One column for each feature of the task.
#' * One column for each performance measure.
#' * `features` (character())\cr
#'   Vector of selected feature set.
#' * `task_id` (`character(1)`).
#' * `learner_id` (`character(1)`).
#' * `resampling_id` (`character(1)`).
#'
#' @param x ([mlr3::ResampleResult] | [mlr3::BenchmarkResult]).
#' @param fselect_instance (`logical(1)`)\cr
#'   If `TRUE`, instances are added to the table.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @return [data.table::data.table()].
#'
#' @export
#' @examples
#' # Nested Resampling on Palmer Penguins Data Set
#'
#' # create auto fselector
#' at = auto_fselector(
#'   fselector = fs("random_search"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' resampling_outer = rsmp("cv", folds = 2)
#' rr = resample(tsk("iris"), at, resampling_outer, store_models = TRUE)
#'
#' # extract inner results
#' extract_inner_fselect_results(rr)
extract_inner_fselect_results = function (x, fselect_instance, ...) {
   UseMethod("extract_inner_fselect_results", x)
}

#' @export
extract_inner_fselect_results.ResampleResult = function(x, fselect_instance = FALSE, ...) {
  rr = assert_resample_result(x)
  if (is.null(rr$learners[[1]]$model$fselect_instance)) {
    return(data.table())
  }
  tab = imap_dtr(rr$learners, function(learner, i) {
    data = setalloccol(learner$fselect_result)
    set(data, j = "iteration", value = i)
    if (fselect_instance) set(data, j = "fselect_instance", value = list(learner$fselect_instance))
    data
  })
  tab[, "task_id" := rr$task$id]
  tab[, "learner_id" := rr$learner$id]
  tab[, "resampling_id" := rr$resampling$id]
  cols_x = rr$learners[[1]]$archive$cols_x
  cols_y = rr$learners[[1]]$archive$cols_y
  setcolorder(tab, c("iteration", cols_x, cols_y))
  tab
}

#' @export
extract_inner_fselect_results.BenchmarkResult = function(x, fselect_instance = FALSE, ...) {
  bmr = assert_benchmark_result(x)
  tab = imap_dtr(bmr$resample_results$resample_result, function(rr, i) {
     data = extract_inner_fselect_results(rr, fselect_instance = fselect_instance)
     if (nrow(data) > 0) set(data, j = "experiment", value = i)
  }, .fill = TRUE)
  # reorder dt
  if (nrow(tab) > 0) {
    cols_x = unique(unlist(map(unique(tab$experiment), function(i) bmr$resample_results$resample_result[[i]]$learners[[1]]$archive$cols_x)))
    cols_y = unique(unlist(map(unique(tab$experiment), function(i) bmr$resample_results$resample_result[[i]]$learners[[1]]$archive$cols_y)))
    setcolorder(tab, unique(c("experiment", "iteration", cols_x, cols_y)))
  }
  tab
}
