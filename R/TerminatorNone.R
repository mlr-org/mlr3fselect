#' @title Terminator that never stops.
#'
#' @aliases mlr_terminators_none
#' @include Terminator.R
#'
#' @description
#' Mainly useful for optimization algorithms, where the stopping is inherently controlled by the algorithm itself (e.g. grid search).
#'
#' @export
TerminatorNone = R6Class("TerminatorNone",
  inherit = Terminator,
  public = list(

    #' @description
    #' Create new `TerminatorNone` object.
    #' @return A `TerminatorNone` object.
    initialize = function() {
      super$initialize()
    },

    #' @description
    #' Is always `FALSE`.
    #' @param instance object of class [FSelect].
    #' @return `logical(1)`
    is_terminated = function(instance) return(FALSE)
  )
)

mlr_terminators$add("none", TerminatorNone)
