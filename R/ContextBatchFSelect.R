#' @title Evaluation Context
#'
#' @description
#' The [ContextBatchFSelect] allows [CallbackBatchFSelect]s to access and modify data while a batch of feature sets is evaluated.
#' See the section on active bindings for a list of modifiable objects.
#' See [callback_batch_fselect()] for a list of stages that access [ContextBatchFSelect].
#'
#' @details
#' This context is re-created each time a new batch of feature sets is evaluated.
#' Changes to `$objective_fselect`, `$design` `$benchmark_result` are discarded after the function is finished.
#' Modification on the data table in `$aggregated_performance` are written to the archive.
#' Any number of columns can be added.
#'
#' @export
ContextBatchFSelect = R6Class("ContextBatchFSelect",
  inherit = ContextBatch,
  public = list(

    #' @field auto_fselector ([AutoFSelector])\cr
    #' The [AutoFSelector] instance.
    auto_fselector = NULL
  ),

  active = list(
    #' @field xss (list())\cr
    #'   The feature sets of the latest batch.
    xss = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.xss)
      } else {
        get_private(self$instance$objective)$.xss = rhs
      }
    },

    #' @field design ([data.table::data.table])\cr
    #'   The benchmark design of the latest batch.
    design = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.design)
      } else {
        get_private(self$instance$objective)$.design = rhs
      }
    },

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
    #'   The benchmark result of the latest batch.
    benchmark_result = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.benchmark_result)
      } else {
        get_private(self$instance$objective)$.benchmark_result = rhs
      }
    },

    #' @field aggregated_performance ([data.table::data.table])\cr
    #'   Aggregated performance scores and training time of the latest batch.
    #'   This data table is passed to the archive.
    #'   A callback can add additional columns which are also written to the archive.
    aggregated_performance = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$instance$objective)$.aggregated_performance)
      } else {
        get_private(self$instance$objective)$.aggregated_performance = rhs
      }
    }
  )
)
