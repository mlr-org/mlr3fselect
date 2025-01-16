#' @title Class for Logging Evaluated Feature Sets
#'
#' @description
#' The [ArchiveBatchFSelect] stores all evaluated feature sets and performance scores.
#'
#' @details
#' The [ArchiveBatchFSelect] is a container around a [data.table::data.table()].
#' Each row corresponds to a single evaluation of a feature set.
#' See the section on Data Structure for more information.
#' The archive stores additionally a [mlr3::BenchmarkResult] (`$benchmark_result`) that records the resampling experiments.
#' Each experiment corresponds to a single evaluation of a feature set.
#' The table (`$data`) and the benchmark result (`$benchmark_result`) are linked by the `uhash` column.
#' If the archive is passed to `as.data.table()`, both are joined automatically.
#'
#' @section Data structure:
#'
#' The table (`$data`) has the following columns:
#'
#' * One column for each feature of the task (`$search_space`).
#' * One column for each performance measure (`$codomain`).
#' * `runtime_learners` (`numeric(1)`)\cr
#'   Sum of training and predict times logged in learners per [mlr3::ResampleResult] / evaluation.
#'   This does not include potential overhead time.
#' * `timestamp` (`POSIXct`)\cr
#'   Time stamp when the evaluation was logged into the archive.
#' * `batch_nr` (`integer(1)`)\cr
#'   Feature sets are evaluated in batches. Each batch has a unique batch number.
#' * `uhash` (`character(1)`)\cr
#'   Connects each feature set to the resampling experiment stored in the [mlr3::BenchmarkResult].
#'
#' @section Analysis:
#' For analyzing the feature selection results, it is recommended to pass the archive to `as.data.table()`.
#' The returned data table is joined with the benchmark result which adds the [mlr3::ResampleResult] for each feature set.
#'
#' The archive provides various getters (e.g. `$learners()`) to ease the access.
#' All getters extract by position (`i`) or unique hash (`uhash`).
#' For a complete list of all getters see the methods section.
#'
#' The benchmark result (`$benchmark_result`) allows to score the feature sets again on a different measure.
#' Alternatively, measures can be supplied to `as.data.table()`.
#'
#' @section S3 Methods:
#' * `as.data.table.ArchiveBatchFSelect(x, exclude_columns = "uhash", measures = NULL)`\cr
#' Returns a tabular view of all evaluated feature sets.\cr
#' [ArchiveBatchFSelect] -> [data.table::data.table()]\cr
#'     * `x` ([ArchiveBatchFSelect])
#'     * `exclude_columns` (`character()`)\cr
#'       Exclude columns from table. Set to `NULL` if no column should be excluded.
#'     * `measures` (list of [mlr3::Measure])\cr
#'       Score feature sets on additional measures.
#'
#' @template param_ties_method
#' @template param_xdt
#' @template param_ydt
#'
#' @export
ArchiveBatchFSelect = R6Class("ArchiveBatchFSelect",
  inherit = ArchiveBatch,

  public = list(

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #' Benchmark result.
    benchmark_result = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param search_space ([paradox::ParamSet])\cr
    #'   Search space.
    #'   Internally created from provided [mlr3::Task] by instance.
    #'
    #' @param codomain ([bbotk::Codomain])\cr
    #'   Specifies codomain of objective function i.e. a set of performance measures.
    #'   Internally created from provided [mlr3::Measure]s by instance.
    #'
    #' @param check_values (`logical(1)`)\cr
    #'   If `TRUE` (default), hyperparameter configurations are check for validity.
    initialize = function(
      search_space,
      codomain,
      check_values = TRUE,
      ties_method = "least_features"
      ) {
      super$initialize(search_space, codomain, check_values)
      self$ties_method = ties_method

      # initialize empty benchmark result
      self$benchmark_result = BenchmarkResult$new()
    },

    #' @description
    #' Adds function evaluations to the archive table.
    #'
    #' @param xss_trafoed (`list()`)\cr
    #'   Ignored in feature selection.
    add_evals = function(xdt, xss_trafoed = NULL, ydt) {
      super$add_evals(xdt = xdt, ydt = ydt)
    },

    #' @description
    #' Retrieve [mlr3::Learner] of the i-th evaluation, by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #' Learner does not contain a model. Use `$learners()` to get learners with models.
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
    },

    #' @description
    #' Returns the best scoring feature sets.
    #'
    #' @param batch (`integer()`)\cr
    #' The batch number(s) to limit the best results to.
    #' Default is all batches.
    #' @param ties_method (`character(1)`)\cr
    #' Method to handle ties.
    #' If `NULL` (default), the global ties method set during initialization is used.
    #' The default global ties method is `least_features` which selects the feature set with the least features.
    #' If there are multiple best feature sets with the same number of features, one is selected randomly.
    #' The `random` method returns a random feature set from the best feature sets.
    #
    #' @return [data.table::data.table()]
    best = function(batch = NULL, ties_method = NULL) {
      ties_method = assert_choice(ties_method, c("least_features", "random"), null.ok = TRUE) %??% self$ties_method
      assert_subset(batch, seq_len(self$n_batch))
      if (self$n_batch == 0L) return(data.table())

      tab = if (is.null(batch)) self$data else self$data[list(batch), , on = "batch_nr"]

      if (self$codomain$target_length == 1L) {
        y = tab[[self$cols_y]] * -self$codomain$direction
        if (ties_method == "least_features") {
          ii = which(y == max(y))
          tab = tab[ii]
          ii = which_min(rowSums(tab[, self$cols_x, with = FALSE]), ties_method = "random")
          tab[ii]
        } else {
          ii = which_max(y, ties_method = "random")
          tab[ii]
        }
      } else {
        ymat = t(as.matrix(tab[, self$cols_y, with = FALSE]))
        ymat = self$codomain$direction * ymat
        tab[!is_dominated(ymat)]
      }
    }
  ),

  active = list(

    #' @field ties_method (`character(1)`)\cr
    #' Method to handle ties.
    ties_method = function(rhs) {
      if (!missing(rhs)) {
        assert_choice(rhs, c("least_features", "random"))
        private$.ties_method = rhs
      } else {
        private$.ties_method
      }
    }
  ),

  private = list(
    .ties_method = NULL
  )
)

#' @export
as.data.table.ArchiveBatchFSelect = function(x, ..., exclude_columns = "uhash", measures = NULL) {
  if (nrow(x$data) == 0) return(data.table())
  # default value for exclude_columns might be not present in archive
  if (!x$benchmark_result$n_resample_results) exclude_columns = exclude_columns[exclude_columns %nin% "uhash"]
  cols_y_extra = NULL
  tab = copy(x$data)

  # add feature vector
  tab[, "features" := lapply(transpose(.SD), function(col) x$cols_x[col]), .SDcols = x$cols_x]
  tab[, "n_features" := map(get("features"), length)]

  if (x$benchmark_result$n_resample_results) {
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
