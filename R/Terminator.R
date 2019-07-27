#' @title Terminator Base Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description This is the base class for terminators.
#'
#' @section Construction:
#' ```
#' fs = Terminator$new(settings)
#' ```
#'
#' * `settings` :: named `list()`\cr Arbitrary settings required by the child class.
#'
#' @section Fields:
#' * `settings` :: named `list()`\cr Settings passed during construction.
#'
#' * `terminated` :: `logical(1)`\cr Is `TRUE` if the termination criterion if the child class is met.
#'
#' * `state` :: `list()`\cr Arbitrary state, depending on the child class.
#'
#' @section Methods:
#' * `update_start(pe, measure)`\cr
#'   [PerformanceEvaluator] -> `self`\cr
#'   [mlr3::Measure] -> `self`\cr
#'   Is called in each feature selection iteration before the evaluation.
#'
#' * `update_end(pe, measure)`\cr
#'   [PerformanceEvaluator] -> `self`\cr
#'   [mlr3::Measure] -> `self`\cr
#'   Is called in each feature selection iteration after the evaluation.
#'
#' @name Terminator
#' @family Terminator
#' @export
Terminator = R6Class("Terminator",
  public = list(
    terminated = NULL,
    settings = NULL,
    state = NULL,

    initialize = function(settings) {
      self$settings = checkmate::assert_list(settings, names = "unique")
    },

    update_start = function(pe, measure) {
      stop("$update_start() not implemented for Terminator")
    },
    update_end = function(pe, measure) {
      stop("$update_end() not implemented for Terminator")
    }
  )
)
