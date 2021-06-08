#' @title Extract Inner Feature Selection Archives
#'
#' @description
#' Extract inner feature selection archives of nested resampling. Implemented for
#' [mlr3::ResampleResult] and [mlr3::BenchmarkResult]. The function iterates
#' over the [AutoFSelector] objects and binds the archives to a
#' [data.table::data.table()]. [AutoFSelector] must be initialized with
#' `store_fselect_instance = TRUE` and `resample()` or `benchmark()` must be
#' called with `store_models = TRUE`.
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
#' * `runtime_learners` (`numeric(1)`)\cr
#'   Sum of training and predict times logged in learners per
#'   [mlr3::ResampleResult] / evaluation. This does not include potential
#'   overhead time. 
#' * `timestamp` (`POSIXct`)\cr
#'   Time stamp when the evaluation was logged into the archive.
#' * `batch_nr` (`integer(1)`)\cr
#'   Feature sets are evaluated in batches. Each batch has a unique batch
#'   number.
#' * `resample_result` ([mlr3::ResampleResult])\cr
#'   Resample result of the inner resampling.
#' * `task_id` (`character(1)`).
#' * `learner_id` (`character(1)`).
#' * `resampling_id` (`character(1)`).
#'
#' @param x ([mlr3::ResampleResult] | [mlr3::BenchmarkResult]).
#' @param unnest (`character()`)\cr
#'   Transforms list columns to separate columns. Set to `NULL` if no column 
#'   should be unnested.
#' @param exclude_columns (`character()`)\cr
#'   Exclude columns from result table. Set to `NULL` if no column should be
#'   excluded.
#' @return [data.table::data.table()].
#'
#' @export
#' @examples
#' at = auto_fselector(
#'   method = "random_search",
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 4)
#'
#' resampling_outer = rsmp("cv", folds = 2)
#' rr = resample(tsk("iris"), at, resampling_outer, store_models = TRUE)
#'
#' extract_inner_fselect_archives(rr)
extract_inner_fselect_archives = function (x, unnest = NULL, exclude_columns = "uhash") {
   UseMethod("extract_inner_fselect_archives")
}

#' @export
extract_inner_fselect_archives.ResampleResult = function(x, unnest = NULL, exclude_columns = "uhash") {
  rr = assert_resample_result(x)
  if (is.null(rr$learners[[1]]$model$fselect_instance)) {
    return(data.table())
  }
  tab = imap_dtr(rr$learners, function(learner, i) {
    data = as.data.table(learner$archive, unnest, exclude_columns)
    set(data, j = "iteration", value = i)
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
extract_inner_fselect_archives.BenchmarkResult = function(x, unnest = NULL, exclude_columns = "uhash") {
  bmr = assert_benchmark_result(x)
  tab = imap_dtr(bmr$resample_results$resample_result, function(rr, i) {
     data = extract_inner_fselect_archives(rr, unnest, exclude_columns)
     if (nrow(data) > 0) set(data, j = "experiment", value = i)
  }, .fill = TRUE)

  if (nrow(tab) > 0) {
    # reorder dt
    cols_x = unique(unlist(map(unique(tab$experiment), function(i) bmr$resample_results$resample_result[[i]]$learners[[1]]$archive$cols_x)))
    cols_y = unique(unlist(map(unique(tab$experiment), function(i) bmr$resample_results$resample_result[[i]]$learners[[1]]$archive$cols_y)))
    setcolorder(tab, c("experiment", "iteration", cols_x, cols_y))
  }
  tab
}