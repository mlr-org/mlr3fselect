#' @title Ensemble Feature Selection Result
#'
#' @description
#' The `EnsembleFSResult` stores the results of the ensemble feature selection.
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

    #' @field benchmark_result (`BenchmarkResult`)\cr
    #' The benchmark result.
    benchmark_result = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param benchmark_result (`BenchmarkResult`)\cr
    #'  The benchmark result object.
    #' @param result ([data.table::data.table])\cr
    #'  The result of the ensemble feature selection results.
    initialize = function(benchmark_result, result) {
      self$benchmark_result = assert_benchmark_result(benchmark_result)
      private$.result = assert_data_table(result)
    },

    #' @description
    #' Calculates the feature ranking.
    #'
    #' @param method (`character(1)`)\cr
    #' The method to calculate the feature ranking.
    #' Currently, only `"inclusion_probability"` is supported.
    feature_ranking = function(method = "inclusion_probability") {
      assert_choice(method, choices = "inclusion_probability")

      features = self$benchmark_result$tasks$task[[1]]$feature_names

      count = map_int(features, function(feature) {
        sum(map_lgl(self$result$features, function(iteration) {
          feature %in% iteration
        }))
      })

      res = data.table(feature = features, inclusion_probability = count / nrow(self$result))
      setorderv(res, "inclusion_probability", order = -1L)

      res
    },

    #' @description
    #' Calculates the stability of the selected features with the `stabm` package.
    #'
    #' @param stability_measure (`character(1)`)\cr
    #'  The stability measure to be used.
    #'  One of the measures returned by [stabm::listStabilityMeasures()] in lower case.
    #'  Default is `"jaccard"`.
    #' @param ... (`any`)\cr
    #'  Additional arguments passed to the stability measure function.
    stability = function(stability_measure = "jaccard", ...) {
      funs = stabm::listStabilityMeasures()$Name
      keys =  tolower(gsub("stability", "", funs))
      assert_choice(stability_measure, choices = keys)

      fun = get(funs[which(stability_measure == keys)], envir = asNamespace("stabm"))
      fun(self$result$features, ...)
    }
  ),

  active = list(

    #' @field result ([data.table::data.table])\cr
    #' Returns the result of the ensemble feature selection.
    result = function(rhs) {
      assert_ro_binding(rhs)
      tab = as.data.table(self$benchmark_result)[, c("task", "learner", "resampling"), with = FALSE]
      cbind(private$.result, tab)
    }
  ),

  private = list(
    .result = NULL
  )
)

#' @export
as.data.table.EnsembleFSResult = function(x, ...) {
  x$result
}
