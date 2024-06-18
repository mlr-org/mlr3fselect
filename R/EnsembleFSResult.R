#' @title Ensemble Feature Selection Result
#'
#' @name ensemble_fs_result
#'
#' @description
#' The `EnsembleFSResult` stores the results of ensemble feature selection.
#' It includes methods for evaluating the stability of the feature selection process and for ranking the selected features.
#' The function [ensemble_fselect()] returns an object of this class.
#'
#' @section S3 Methods:
#' * `as.data.table.EnsembleFSResult(x, benchmark_result = TRUE)`\cr
#' Returns a tabular view of the ensemble feature selection.\cr
#' [EnsembleFSResult] -> [data.table::data.table()]\cr
#'     * `x` ([EnsembleFSResult])
#'     * `benchmark_result` (`logical(1)`)\cr
#'       Whether to add the learner, task and resampling information from the benchmark result.
#' @export
#' @examples
#' \donttest{
#'   efsr = ensemble_fselect(
#'     fselector = fs("rfe", n_features = 2, feature_fraction = 0.8),
#'     task = tsk("sonar"),
#'     learners = lrns(c("classif.rpart", "classif.featureless")),
#'     init_resampling = rsmp("subsampling", repeats = 2),
#'     inner_resampling = rsmp("cv", folds = 3),
#'     measure = msr("classif.ce"),
#'     terminator = trm("none")
#'   )
#'
#'   # contains the benchmark result
#'   efsr$benchmark_result
#'
#'   # contains the selected features for each iteration
#'   efsr$result
#'
#'   # returns the stability of the selected features
#'   efsr$stability(stability_measure = "jaccard")
#'
#'   # returns a ranking of all features
#'   head(efsr$feature_ranking())
#' }
EnsembleFSResult = R6Class("EnsembleFSResult",
  public = list(

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #' The benchmark result.
    benchmark_result = NULL,

    #' @field man (`character(1)`)\cr
    #' Manual page for this object.
    man = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param result ([data.table::data.table])\cr
    #'  The result of the ensemble feature selection.
    #'  Column names should include `"resampling_iteration"`, `"learner_id"`, `"features"`
    #'  and `"n_features"`.
    #' @param features ([character()])\cr
    #'  The vector of features of the task that was used in the ensemble feature
    #'  selection.
    #' @param benchmark_result ([mlr3::BenchmarkResult])\cr
    #'  The benchmark result object.
    #' @param measure_var (`character(1)`)\cr
    #'  Column name of `"result"` that corresponds to the measure used.
    #' @param minimize (`logical(1)`)\cr
    #'  If `TRUE` (default), lower values of the measure correspond to higher performance.
    initialize = function(result, features, benchmark_result = NULL, measure_var,
                          minimize = TRUE) {
      assert_data_table(result)
      private$.measure_var = assert_string(measure_var, null.ok = FALSE)
      mandatory_columns = c("resampling_iteration", "learner_id", "features", "n_features")
      assert_names(names(result), must.include = c(mandatory_columns, measure_var))
      private$.result = result
      private$.features = assert_character(features, any.missing = FALSE, null.ok = FALSE)
      private$.minimize = assert_logical(minimize, null.ok = FALSE)
      self$benchmark_result = if (!is.null(benchmark_result)) assert_benchmark_result(benchmark_result)

      self$man = "mlr3fselect::ensemble_fs_result"
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function(...) {
      catf(format(self))
      print(private$.result[, c("resampling_iteration", "learner_id", "n_features"), with = FALSE])
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Calculates the feature ranking.
    #'
    #' @details
    #' The feature ranking process is built on the following framework: models act as voters, features act as candidates, and voters select certain candidates (features).
    #' The primary objective is to compile these selections into a consensus ranked list of features, effectively forming a committee.
    #' Currently, only `"approval_voting"` method is supported, which selects the candidates/features that have the highest approval score or selection frequency, i.e. appear the most often.
    #'
    #' @param method (`character(1)`)\cr
    #' The method to calculate the feature ranking.
    #'
    #' @return A [data.table::data.table] listing all the features, ordered by decreasing inclusion probability scores (depending on the `method`)
    feature_ranking = function(method = "approval_voting") {
      assert_choice(method, choices = "approval_voting")

      # cached results
      if (!is.null(private$.feature_ranking[[method]])) {
        return(private$.feature_ranking[[method]])
      }

      count_tbl = sort(table(unlist(private$.result$features)), decreasing = TRUE)
      features_selected = names(count_tbl)
      features_not_selected = setdiff(private$.features, features_selected)

      res_fs = data.table(
        feature = features_selected,
        inclusion_probability = as.vector(count_tbl) / nrow(private$.result)
      )

      res_fns = data.table(
        feature = features_not_selected,
        inclusion_probability = 0
      )

      res = rbindlist(list(res_fs, res_fns))

      private$.feature_ranking[[method]] = res
      private$.feature_ranking[[method]]
    },

    #' @description
    #' Calculates the stability of the selected features with the \CRANpkg{stabm} package.
    #' The results are cached.
    #' When the same stability measure is requested again with different arguments, the cache must be reset.
    #'
    #' @param stability_measure (`character(1)`)\cr
    #'  The stability measure to be used.
    #'  One of the measures returned by [stabm::listStabilityMeasures()] in lower case.
    #'  Default is `"jaccard"`.
    #' @param ... (`any`)\cr
    #'  Additional arguments passed to the stability measure function.
    #' @param global (`logical(1)`)\cr
    #'  Whether to calculate the stability globally or for each learner.
    #' @param reset_cache (`logical(1)`)\cr
    #'  If `TRUE`, the cached results are ignored.
    #'
    #' @return A `numeric()` value representing the stability of the selected features.
    #' Or a `numeric()` vector with the stability of the selected features for each learner.
    stability = function(stability_measure = "jaccard", ..., global = TRUE, reset_cache = FALSE) {
      funs = stabm::listStabilityMeasures()$Name
      keys = tolower(gsub("stability", "", funs))
      assert_choice(stability_measure, choices = keys)

      if (global) {
        # cached results
        if (!is.null(private$.stability_global[[stability_measure]]) && !reset_cache) {
          return(private$.stability_global[[stability_measure]])
        }

        fun = get(funs[which(stability_measure == keys)], envir = asNamespace("stabm"))
        private$.stability_global[[stability_measure]] = fun(private$.result$features, ...)
        private$.stability_global[[stability_measure]]
      } else {
        # cached results
        if (!is.null(private$.stability_learner[[stability_measure]]) && !reset_cache) {
          return(private$.stability_learner[[stability_measure]])
        }

        fun = get(funs[which(stability_measure == keys)], envir = asNamespace("stabm"))

        tab = private$.result[, list(score = fun(.SD$features, ...)), by = learner_id]
        private$.stability_learner[[stability_measure]] = set_names(tab$score, tab$learner_id)
        private$.stability_learner[[stability_measure]]
      }
    }
  ),

  active = list(

    #' @field result ([data.table::data.table])\cr
    #' Returns the result of the ensemble feature selection.
    result = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(self$benchmark_result)) return(private$.result)
      tab = as.data.table(self$benchmark_result)[, c("task", "learner", "resampling"), with = FALSE]
      cbind(private$.result, tab)
    },

    #' @field nlearners (`numeric(1)`)\cr
    #' Returns the number of learners used in the ensemble feature selection.
    nlearners = function(rhs) {
      assert_ro_binding(rhs)
      uniqueN(private$.result$learner_id)
    },

    #' @field measure (`character(1)`)\cr
    #' Returns the measure id used in the ensemble feature selection.
    measure = function(rhs) {
      assert_ro_binding(rhs)
      private$.measure_var
    }
  ),

  private = list(
    .result = NULL, # with no R6 classes
    .stability_global = NULL,
    .stability_learner = NULL,
    .feature_ranking = NULL,
    .features = NULL,
    .measure_var = NULL,
    .minimize = NULL
  )
)

#' @export
as.data.table.EnsembleFSResult = function(x,  ...) {
  x$result
}
