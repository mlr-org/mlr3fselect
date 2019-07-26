#' @title TerminatorPerformanceStep
#'
#' @format [R6::R6Class] inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Feature selection terminates depending on the performance improvement between the current and previous evaluation set.
#'
#' @section Construction:
#'  ```
#' tm = TerminatorPerformanceStep$new(threshold)
#' ```
#'
#' * `threshold` (`numeric(1)``)\cr
#'
#' @section Fields:
#' See [Terminator].
#'
#' @section Methods:
#' See [Terminator].
#'
#' @family Terminator
#' @examples
#' tm = TerminatorPerformanceStep$new(threshold = 0.01)
#' @export
TerminatorPerformanceStep = R6Class("TerminatorPerformanceStep",
  inherit = Terminator,
  public = list(
    initialize = function(threshold) {
      super$initialize(
        settings = list(threshold = checkmate::assert_numeric(threshold)))

      self$terminated = FALSE
      self$state = list(step_performance = NA)
    },

    update_start = function(pe, measure) {
      invisible(self)
    },
    update_end = function(pe, measure) {
      bmr = pe$bmr[[length(pe$bmr)]]
      bmr_best = bmr$best(measure)
      performance_best = bmr_best$aggregate(measure)

      if (!is.na(self$state$step_performance)) {
        if (measure$minimize) {
          if (self$state$step_performance - performance_best <= self$settings$threshold) {
            self$terminated = TRUE
          }
        } else {
          if (performance_best - self$state$step_performance <= self$settings$threshold) {
            self$terminated = TRUE
          }
        }
      }
      self$state$step_performance = performance_best
      invisible(self)
    }
  )
)
