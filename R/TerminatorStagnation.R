#' @title Terminator that stops when tuning does not improve
#'
#' @aliases mlr_terminators_stagnation
#' @include Terminator.R
#'
#' @description
#' Class to terminate the optimization after the performance stagnates, i.e. does not improve more than
#' `threshold` over the last `iters` iterations.
#'
#' @section Parameters:
#' * `iters` :: `integer(1)`\cr
#'   Number of iterations to evaluate the performance improvement on, default is 10.
#'
#' * `threshold` :: `numeric(1)`\cr
#'   If the improvement is less than `threshold`, tuning is stopped, default is `0`.
#'
#' @export
#' @examples
#' TerminatorStagnation$new()
#' term("stagnation", iters = 5, threshold = 1e-5)
TerminatorStagnation = R6Class("TerminatorStagnation",
  inherit = Terminator,
  public = list(

    #' @description
    #' Create new `TerminatorStagnation` object.
    #' @return A `TerminatorStagnation` object.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("iters", lower = 1L, default = 10, tags = "required"),
        ParamDbl$new("threshold", lower = 0, default = 0, tags = "required")
      ))
      ps$values = list(iters = 10, threshold = 0)
      super$initialize(param_set = ps)
    },

    #' @description
    #' Is `TRUE` after the performance stagnates, and `FALSE` otherwise.
    #' @param instance object of class [FSelect].
    #' @return `logical(1)`
    is_terminated = function(instance) {
      pv = self$param_set$values
      iters = pv$iters

      aggr = instance$archive(unnest = "no")
      if (nrow(aggr) <= iters) {
        return(FALSE)
      }

      m = instance$measures[[1L]]

      perf_before = head(aggr[[m$id]], -iters)
      perf_window = tail(aggr[[m$id]],  iters)

      if (m$minimize) {
        min(perf_window) >= min(perf_before) - pv$threshold
      } else {
        max(perf_window) <= max(perf_before) + pv$threshold
      }
    }
  )
)

mlr_terminators$add("stagnation", TerminatorStagnation)
