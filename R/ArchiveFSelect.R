#' @title Logging Object for Evaluated Feature Sets
#'
#' @description
#' Container around a [data.table::data.table()] which stores all evaluated
#' feature sets and performance scores.
#'
#' @section Data structure:
#'
#' The table (`$data`) has the following columns:
#'
#' * One column for each feature of the task (`$search_space`).
#' * One column for each performance measure (`$codomain`).
#' * `runtime_learners` (`numeric(1)`)\cr
#'   Sum of training and predict times logged in learners per
#'   [mlr3::ResampleResult] / evaluation. This does not include potential
#'   overhead time. 
#' * `timestamp` (`POSIXct`)\cr
#'   Time stamp when the evaluation was logged into the archive.
#' * `batch_nr` (`integer(1)`)\cr
#'   Feature sets are evaluated in batches. Each batch has a unique batch
#'   number.
#' * `uhash` (`character(1)`)\cr
#'   Connects each feature set to the resampling experiment
#'   stored in the [mlr3::BenchmarkResult].
#'
#' Each row corresponds to a single evaluation of a feature set.
#'
#' The archive stores additionally a [mlr3::BenchmarkResult]
#' (`$benchmark_result`) that records the resampling experiments. Each
#' experiment corresponds to to a single evaluation of a feature set. The table
#' (`$data`) and the benchmark result (`$benchmark_result`) are linked by the
#' `uhash` column. If the results are viewed with `as.data.table()`, both are
#' joined automatically.
#' 
#' @section Analysis:
#'
#' For analyzing the feature selection results, it is recommended to pass the archive to
#' `as.data.table()`. The returned data table is joined with the benchmark result
#' which adds the [mlr3::ResampleResult] for each feature set.
#'
#' The archive provides various getters (e.g. `$learners()`) to ease the access.
#' All getters extract by position (`i`) or unique hash (`uhash`). For a
#' complete list of all getters see the methods section.
#'
#' The benchmark result (`$benchmark_result`) allows to score the feature sets
#' again on a different measure. Alternatively, measures can be supplied to
#' `as.data.table()`.
#' 
#' @section S3 Methods:
#' * `as.data.table.ArchiveFSelect(x, unnest = NULL, exclude_columns = "uhash", measures = NULL)`\cr
#' Returns a tabular view of all evaluated feature sets.\cr
#' [ArchiveFSelect] -> [data.table::data.table()]\cr
#'     * `x` ([ArchiveFSelect])
#'     * `unnest` (`character()`)\cr
#'       Transforms list columns to separate columns. Set to `NULL` if no column
#'       should be unnested.
#'     * `exclude_columns` (`character()`)\cr
#'       Exclude columns from table. Set to `NULL` if no column should be
#'       excluded.
#'     * `measures` (list of [mlr3::Measure])\cr
#'       Score feature sets on additional measures.
#'
#' @export
ArchiveFSelect = R6Class("ArchiveFSelect",
  inherit = Archive,

  public = list(

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #' Stores benchmark result.
    benchmark_result = NULL,

    #' @description
    #' Retrieve [mlr3::Learner] of the i-th evaluation, by position
    #' or by unique hash `uhash`. `i` and `uhash` are mutually exclusive.
    #' Learner does not contain a model. Use `$learners()` to get learners with
    #' models.
    #'
    #' @param i (`integer(1)`)\cr
    #' The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #' The `uhash` value to filter for.
    learner = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$learner
    },

    #' @description
    #' Retrieve list of trained [mlr3::Learner] objects of the i-th evaluation,
    #' by position or by unique hash `uhash`. `i` and `uhash` are mutually
    #' exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #' The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #' The `uhash` value to filter for.
    learners = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$learners
    },

    #' @description
    #' Retrieve list of [mlr3::Prediction] objects of the i-th evaluation, by
    #' position or by unique hash `uhash`. `i` and `uhash` are mutually
    #' exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #' The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #' The `uhash` value to filter for.
    predictions = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$predictions()
    },

    #' @description
    #' Retrieve [mlr3::ResampleResult] of the i-th evaluation, by position
    #' or by unique hash `uhash`. `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #' The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #' The `uhash` value to filter for.
    resample_result = function(i = NULL, uhash = NULL) {
      self$benchmark_result$resample_result(i = i, uhash = uhash)
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function() {
      catf(format(self))
      print(self$data[, setdiff(names(self$data), "uhash"), with = FALSE], digits=2)
    }
  )
)

#' @export
as.data.table.ArchiveFSelect = function(x, ..., unnest = NULL, exclude_columns = "uhash", measures = NULL) {
  if (nrow(x$data) == 0) return(data.table())
  # default value for exclude_columns might be not present in archive
  if (is.null(x$benchmark_result)) exclude_columns = exclude_columns[exclude_columns %nin% "uhash"]
 
  assert_subset(unnest, names(x$data))
  cols_y_extra = NULL
  
  # unnest data
  tab = unnest(copy(x$data), unnest, prefix = "{col}_")

  if (!is.null(x$benchmark_result)) {
    # add extra measures
    if (!is.null(measures)) {
      measures = assert_measures(as_measures(measures), learner = x$learners(1)[[1]], task = x$resample_result(1)$task)
      cols_y_extra = map_chr(measures, "id")
      tab = cbind(tab, x$benchmark_result$aggregate(measures)[, cols_y_extra, with = FALSE])
    }
    # add resample results
    tab = merge(tab, x$benchmark_result$resample_results[, c("uhash", "resample_result"), with = FALSE], by = "uhash", sort = FALSE)
  }
  setcolorder(tab, c(x$cols_x, x$cols_y, cols_y_extra, "runtime_learners", "timestamp", "batch_nr"))
  assert_subset(exclude_columns, names(tab))
  tab[, setdiff(names(tab), exclude_columns), with = FALSE]
}