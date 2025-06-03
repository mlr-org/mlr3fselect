#' @title Asynchronous Feature Selection Context
#'
#' @description
#' A [CallbackAsyncFSelect] accesses and modifies data during the optimization via the `ContextAsyncFSelect`.
#' See the section on active bindings for a list of modifiable objects.
#' See [callback_async_fselect()] for a list of stages that access `ContextAsyncFSelect`.
#'
#' @details
#' Changes to `$instance` and `$optimizer` in the stages executed on the workers are not reflected in the main process.
#'
#' @template param_inst_async
#' @template param_fselector
#'
#' @export
ContextAsyncFSelect = R6Class("ContextAsyncFSelect",
  inherit = ContextAsync,
  public = list(

    #' @field auto_fselector ([AutoFSelector])\cr
    #' The [AutoFSelector] instance.
    auto_fselector = NULL
  ),

  active = list(

    #' @field resample_result ([mlr3::BenchmarkResult])\cr
    #' The resample result of the feature subset currently evaluated.
    resample_result = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.resample_result)
      } else {
        self$instance$objective$.__enclos_env__$private$.resample_result = rhs
      }
    },

    #' @field aggregated_performance (`list()`)\cr
    #' Aggregated performance scores and training time of the evaluated feature subset.
    #' This list is passed to the archive.
    #' A callback can add additional elements which are also written to the archive.
    aggregated_performance = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.aggregated_performance)
      } else {
        self$instance$objective$.__enclos_env__$private$.aggregated_performance = rhs
      }
    },

    #' @field result_feature_set (character())\cr
    #' The feature set passed to `instance$assign_result()`.
    result_feature_set = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance)$.result_feature_set)
      } else {
        self$instance$.__enclos_env__$private$.result_feature_set = rhs
      }
    }
  )
)
