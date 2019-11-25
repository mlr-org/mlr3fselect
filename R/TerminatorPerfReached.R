#' @title Terminator that stops when a performance level has been reached
#'
#' @aliases mlr_terminators_perf_reached
#' @include Terminator.R
#'
#' @description
#' Class to terminate the tuning after a performance level has been hit.
#'
#' @section Parameters:
#' * `level` :: `numeric(1)`\cr
#'   Performance level that needs to be reached, default is 0.
#'   Terminates if the performance exceeds (respective measure has to be maximized) or
#'   falls below (respective measure has to be minimized) this value.
#'
#' @export
#' @examples
#' TerminatorPerfReached$new()
#' term("perf_reached")
TerminatorPerfReached = R6Class("TerminatorPerfReached",
  inherit = Terminator,
  public = list(

    #' @description
    #' Create new `TerminatorPerfReached` object.
    #' @return A `TerminatorPerfReached` object.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamDbl$new("level", default = 0, tags = "required")
      ))
      ps$values = list(level = 0)
      super$initialize(param_set = ps)
    },

    #' @description
    #' Is `TRUE` after a performance level has been hit, and `FALSE` otherwise.
    #' @param instance object of class [FSelect] or [Tuner].
    #' @return `logical(1)`
    is_terminated = function(instance) {
      pv = self$param_set$values
      m = instance$measures[[1L]]
      aggr = instance$archive(unnest = "no")
      if (m$minimize) {
        any(aggr[[m$id]] <= pv$level)
      } else {
        any(aggr[[m$id]] >= pv$level)
      }
    }
  )
)

mlr_terminators$add("perf_reached", TerminatorPerfReached)
