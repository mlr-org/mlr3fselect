#' @title Terminator that stops after a number of evaluations
#'
#' @aliases mlr_terminators_evals
#' @include Terminator.R
#'
#' @description
#' Class to terminate the optimization depending on the number of evaluations.
#' An evaluation is defined by one resampling of a parameter value.
#'
#' @section Parameters:
#' * `n_evals` :: `integer(1)`\cr
#'   Number of allowed evaluations, default is 100L
#'
#' @family Terminator
#' @export
#' @examples
#' TerminatorEvals$new()
#' term("evals", n_evals = 5)
TerminatorEvals = R6Class("TerminatorEvals",
  inherit = Terminator,
  public = list(

    #' @description
    #' Create new `TerminatorEvals` object.
    #' @return A `TerminatorEvals` object.
    initialize = function() {
      ps = ParamSet$new(list(ParamInt$new("n_evals", lower = 1L, default = 100L, tags = "required")))
      ps$values = list(n_evals = 100L)

      super$initialize(param_set = ps)
    },

    #' @description
    #' Is `TRUE` depending on the number of evaluations, and `FALSE` otherwise.
    #' @param instance object of class [FSelect] or [Tuner].
    #' @return `logical(1)`
    is_terminated = function(instance) {
      instance$n_evals >= self$param_set$values$n_evals
    }
  )
)

mlr_terminators$add("evals", TerminatorEvals)
