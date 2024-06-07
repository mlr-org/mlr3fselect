#' @title Ensemble Feature Selection Result
#'
#' @name ensemble_fs_result
#'
#' @description
#' The `EnsembleFSResult` stores the results of the ensemble feature selection
#' and incorporates methods for assessing the stability of the feature selection
#' and ranking the features.
#'
#' The function [ensemble_fselect()] returns an object of this class.
#'
#' @examples
#' \donttest{
#' efsr = ensemble_fselect(
#'   fselector = fs("rfe", n_features = 2, feature_fraction = 0.8),
#'   task = tsk("sonar"),
#'   learners = lrns(c("classif.rpart", "classif.featureless")),
#'   init_resampling = rsmp("subsampling", repeats = 2),
#'   inner_resampling = rsmp("cv", folds = 3),
#'   measure = msr("classif.ce"),
#'   terminator = trm("none")
#' )
#'
#' # contains the benchmark result
#' efsr$benchmark_result
#'
#' # contains the selected features for each iteration
#' efsr$result
#'
#' # returns the stability of the selected features
#' efsr$stability(stability_measure = "jaccard")
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
    #' @param benchmark_result ([mlr3::BenchmarkResult])\cr
    #'  The benchmark result object.
    #' @param result ([data.table::data.table])\cr
    #'  The result of the ensemble feature selection results.
    #' @param features ([character()])\cr
    #'  The vector of features of the task that was used in the ensemble feature
    #'  selection. Ignored if `benchmark_result` is given and mandatory to have
    #'  if `benchmark_result` is `NULL`.
    initialize = function(benchmark_result = NULL, result, features) {
      if (is.null(benchmark_result)) {
        assert_character(features, any.missing = FALSE, null.ok = FALSE)
        private$.features = features
      } else {
        self$benchmark_result = assert_benchmark_result(benchmark_result)
        private$.features = self$benchmark_result$tasks$task[[1]]$feature_names
      }

      assert_data_table(result)
      assert_names(names(result), must.include = c("iter", "learner_id", "features", "n_features"))

      private$.result = result
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
      print(self$result[, c("learner_id", "n_features"), with = FALSE])
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Calculates the feature ranking.
    #'
    #' @param method (`character(1)`)\cr
    #' The method to calculate the feature ranking.
    #' Currently, only `"inclusion_probability"` is supported.
    #'
    #' @return A [data.table][data.table::data.table] listing all the features,
    #' ordered by decreasing inclusion probability scores (depending on the
    #' `method`)
    feature_ranking = function(method = "inclusion_probability") {
      assert_choice(method, choices = "inclusion_probability")

      # cached results
      if (!is.null(private$.feature_ranking[[method]])) {
        return(private$.feature_ranking[[method]])
      }

      count_tbl = sort(table(unlist(self$result$features)), decreasing = TRUE)
      features_selected = names(count_tbl)
      features_not_selected = setdiff(private$.features, features_selected)

      res_fs = data.table(
        feature = features_selected,
        inclusion_probability = as.vector(count_tbl) / nrow(self$result)
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
    #' Calculates the stability of the selected features with the `stabm` package.
    #' The results are cached.
    #' When the same stability measure is requested again with different arguments, the cache must be reset.
    #'
    #' @param stability_measure (`character(1)`)\cr
    #'  The stability measure to be used.
    #'  One of the measures returned by [stabm::listStabilityMeasures()] in lower case.
    #'  Default is `"jaccard"`.
    #' @param ... (`any`)\cr
    #'  Additional arguments passed to the stability measure function.
    #' @param reset_cache (`logical(1)`)\cr
    #'  If `TRUE`, the cached results are ignored.
    stability = function(stability_measure = "jaccard", ..., reset_cache = FALSE) {
      funs = stabm::listStabilityMeasures()$Name
      keys = tolower(gsub("stability", "", funs))
      assert_choice(stability_measure, choices = keys)

      # cached results
      if (!is.null(private$.stability[[stability_measure]]) && !reset_cache) {
        return(private$.stability[[stability_measure]])
      }

      fun = get(funs[which(stability_measure == keys)], envir = asNamespace("stabm"))
      private$.stability[[stability_measure]] = fun(self$result$features, ...)
      private$.stability[[stability_measure]]
    }
  ),

  active = list(

    #' @field result ([data.table::data.table])\cr
    #' Returns the result of the ensemble feature selection.
    result = function(rhs) {
      assert_ro_binding(rhs)
      private$.result
    }
  ),

  private = list(
    .result = NULL,
    .stability = NULL,
    .feature_ranking = NULL,
    .features = NULL
  )
)

#' @export
as.data.table.EnsembleFSResult = function(x, ...) {
  x$result
}
