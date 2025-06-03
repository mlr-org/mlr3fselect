#' @title Rush Data Storage
#'
#' @description
#' The `ArchiveAsyncFSelect` stores all evaluated feature subsets and performance scores in a [rush::Rush] database.
#'
#' @details
#' The [ArchiveAsyncFSelect] is a connector to a [rush::Rush] database.
#'
#' @section Data Structure:
#'
#' The table (`$data`) has the following columns:
#'
#' * One column for each feature of the search space (`$search_space`).
#' * One column for each performance measure (`$codomain`).
#' * `x_domain` (`list()`)\cr
#'     Lists of (transformed) feature values that are passed to the learner.
#' * `runtime_learners` (`numeric(1)`)\cr
#'     Sum of training and predict times logged in learners per [mlr3::ResampleResult] / evaluation.
#'     This does not include potential overhead time.
#' * `timestamp` (`POSIXct`)\cr
#'     Time stamp when the evaluation was logged into the archive.
#' * `batch_nr` (`integer(1)`)\cr
#'     Feature subsets are evaluated in batches.
#'     Each batch has a unique batch number.
#'
#' @section Analysis:
#' For analyzing the feature selection results, it is recommended to pass the [ArchiveAsyncFSelect] to `as.data.table()`.
#' The returned data table contains the [mlr3::ResampleResult] for each feature subset evaluation.
#'
#' @section S3 Methods:
#' * `as.data.table.ArchiveFSelect(x, unnest = "x_domain", exclude_columns = "uhash", measures = NULL)`\cr
#' Returns a tabular view of all evaluated feature subsets.\cr
#' [ArchiveAsyncFSelect] -> [data.table::data.table()]\cr
#'     * `x` ([ArchiveAsyncFSelect])
#'     * `unnest` (`character()`)\cr
#'       Transforms list columns to separate columns. Set to `NULL` if no column should be unnested.
#'     * `exclude_columns` (`character()`)\cr
#'       Exclude columns from table. Set to `NULL` if no column should be excluded.
#'     * `measures` (List of [mlr3::Measure])\cr
#'       Score feature subsets on additional measures.
#'
#' @template param_search_space
#' @template param_codomain
#' @template param_rush
#' @template param_ties_method
#'
#' @export
ArchiveAsyncFSelect = R6Class("ArchiveAsyncFSelect",
  inherit = bbotk::ArchiveAsync,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param check_values (`logical(1)`)\cr
    #'   If `TRUE` (default), feature subsets are check for validity.
    initialize = function(
      search_space,
      codomain,
      rush,
      ties_method = "least_features"
      ) {
      super$initialize(
        search_space = search_space,
        codomain = codomain,
        rush = rush)

      private$.benchmark_result = BenchmarkResult$new()
      private$.ties_method = assert_choice(ties_method, c("least_features", "random"))
    },

    #' @description
    #' Retrieve [mlr3::Learner] of the i-th evaluation, by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #' Learner does not contain a model. Use `$learners()` to get learners with models.
    #'
    #' @param i (`integer(1)`)\cr
    #'   The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #'   The `uhash` value to filter for.
    learner = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$learner
    },

    #' @description
    #' Retrieve list of trained [mlr3::Learner] objects of the i-th evaluation, by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #'   The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #'   The `uhash` value to filter for.
    learners = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$learners
    },

    #' @description
    #' Retrieve list of [mlr3::Prediction] objects of the i-th evaluation, by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #'   The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #'   The `uhash` value to filter for.
    predictions = function(i = NULL, uhash = NULL) {
      self$resample_result(i = i, uhash = uhash)$predictions()
    },

    #' @description
    #' Retrieve [mlr3::ResampleResult] of the i-th evaluation, by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #'   The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #'   The `uhash` value to filter for.
    resample_result = function(i = NULL, uhash = NULL) {
      self$benchmark_result$resample_result(i = i, uhash = uhash)
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function() {
      cat_cli(cli_h1("{format(self)} with {.val {self$n_evals}} evaluations"))
      print(as.data.table(self, unnest = NULL, exclude_columns = c(
        "x_domain",
        "timestamp_xs",
        "timestamp_ys",
        "runtime_learners",
        "resample_result",
        "worker_id",
        "keys",
        "pid",
        "state")), digits = 2)
    },

    #' @description
    #' Returns the best scoring feature set(s).
    #' For single-crit optimization, the solution that minimizes / maximizes the objective function.
    #' For multi-crit optimization, the Pareto set / front.
    #'
    #' @param n_select (`integer(1L)`)\cr
    #' Amount of points to select.
    #' Ignored for multi-crit optimization.
    #' @param ties_method (`character(1L)`)\cr
    #' Method to break ties when multiple points have the same score.
    #' Either `"least_features"` (default) or `"random"`.
    #' Ignored for multi-crit optimization.
    #' If `n_select > 1L`, the tie method is ignored and the first point is returned.
    #'
    #' @return [data.table::data.table()]
    best = function(n_select = 1, ties_method = "least_features") {
      ties_method = assert_choice(ties_method, c("least_features", "random"), null.ok = TRUE) %??% private$.ties_method
      assert_count(n_select)
      tab = self$finished_data

      if (self$codomain$target_length == 1L) {
        if (n_select == 1L) {
          # use which_max to find the best point
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
          # use data.table fast sort to find the best points
          setorderv(tab, cols = self$cols_y, order = self$codomain$direction)
          head(tab, n_select)
        }
      } else {
        # use non-dominated sorting to find the best points
        ymat = t(as.matrix(tab[, self$cols_y, with = FALSE]))
        ymat = self$codomain$direction * ymat
        tab[!is_dominated(ymat)]
      }
    }
  ),

  active = list(
    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #' Benchmark result.
    benchmark_result = function() {
      # cache benchmark result
      if (self$rush$n_finished_tasks > private$.benchmark_result$n_resample_results) {
        bmrs = map(self$finished_data$resample_result, as_benchmark_result)
        private$.benchmark_result = Reduce(function(lhs, rhs) lhs$combine(rhs), bmrs)
      }
      private$.benchmark_result
    },

    #' @field ties_method (`character(1)`)\cr
    #' Method to handle ties in the archive.
    #' One of `"least_features"` (default) or `"random"`.
    ties_method = function(rhs) {
      assert_ro_binding(rhs)
      private$.ties_method
    }
  ),

  private = list(
    .benchmark_result = NULL,
    .ties_method = NULL
  )
)

#' @export
as.data.table.ArchiveAsyncFSelect = function(x, ..., unnest = NULL, exclude_columns = NULL, measures = NULL) {
  data = x$data_with_state()
  if (!nrow(data)) return(data.table())

  # unnest columns
  cols = intersect(unnest, names(data))
  tab = unnest(data, cols, prefix = "{col}_")

  # add extra measures
  cols_y_extra = NULL
  if (!is.null(measures) && !is.null(tab$resample_result)) {
    measures = assert_measures(as_measures(measures), learner = x$learners(1)[[1]], task = x$resample_result(1)$task)
    cols_y_extra = map_chr(measures, "id")
    scores = map_dtr(x$data$resample_result, function(rr) as.data.table(as.list(rr$aggregate(measures))))
    tab = cbind(tab, scores)
  }

  setcolorder(tab, c(x$cols_x, x$cols_y, cols_y_extra, cols_x_domain, "runtime_learners", "timestamp_xs", "timestamp_ys"))
  tab[, setdiff(names(tab), exclude_columns), with = FALSE]
}
