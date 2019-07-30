#' @title TerminatorPerformance
#'
#' @format [R6::R6Class] inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Feature selection terminates depending on the performance.
#'
#' @section Construction:
#'  ```
#' tm = TerminatorPerformance$new(threshold)
#' ```
#'
#' * `threshold` :: `numeric(1)`\cr
#'
#' @section Fields:
#' See [Terminator].
#'
#' @section Methods:
#' See [Terminator].
#'
#' @family Terminator
#' @examples
#' tm = TerminatorPerformance$new(threshold = 0.76)
#' @export
TerminatorPerformance = R6Class("TerminatorPerformance",
  inherit = Terminator,
  public = list(
    initialize = function(threshold) {
      super$initialize(
        settings = list(threshold = checkmate::assert_numeric(threshold)))

      self$terminated = FALSE
      self$state = Inf
    },

    update_start = function(pe, measure) {
      invisible(self)
    },
    update_end = function(pe, measure) {
      bmr = pe$bmr[[length(pe$bmr)]]
      bmr_best = bmr$best(measure)
      performance_best = bmr_best$aggregate(measure)

      if (measure$minimize) {
        self$state = performance_best - self$settings$threshold
      } else {
        self$state = self$settings$threshold - performance_best
      }

      if (self$state < 0) {
        self$terminated = TRUE
      }
      invisible(self)
    }
  )
)
