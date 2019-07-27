#' @title TerminatorEvaluations
#'
#' @format [R6::R6Class] inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Feature selection terminates depending on the number of evaluations. An evaluation is defined by one resampling of a feature set.
#'
#' @section Construction:
#'  ```
#' fs = TerminatorEvaluations$new(max_evaluations)
#' ```
#'
#' * `max_evaluations` (`integer(1)`)\cr Maximum number of evaluations.
#'
#' @section Fields:
#' See [Terminator].
#'
#' @section Methods:
#' See [Terminator].
#'
#' @family Terminator
#' @examples
#' tm = TerminatorEvaluations$new(max_evaluations = 100)
#' @export
TerminatorEvaluations = R6Class("TerminatorEvaluations",
  inherit = Terminator,
  public = list(
    initialize = function(max_evaluations) {
      super$initialize(settings = list(max_evaluations = checkmate::assert_count(max_evaluations, positive = TRUE, coerce = TRUE)))

      self$state = list(evals = 0L)
      self$terminated = FALSE
    },

    update_start = function(pe, measure = NA) {
      if (length(pe$bmr) < 1) {
        self$state$evals = 0L
      } else {
        row_num = lapply(pe$bmr, function(bmr) nrow(bmr$aggregate()))
        self$state$evals = Reduce("sum", row_num)
      }

      self$terminated = self$state$evals >= self$settings$max_evaluations

      invisible(self)
    },

    update_end = function(pe, measure = NA) {
      self$update_start(pe)
    }
  )
)
