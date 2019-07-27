#' @title TerminatorRuntime Class
#'
#' @format [R6::R6Class] inheriting from [Terminator].
#' @include Terminator.R
#'
#' @description
#' Feature selection terminates depending on the runtime.
#'
#' @section Construction:
#'  ```
#' tm = TerminatorRuntime$new(max_time, time_unit)
#' ```
#'
#' * `max_time` :: `numeric(1)`\cr Maximum allowed runtime.
#'
#' * `units` :: `character(1)`\cr Unit of the maximum allowed runtime in `secs`, `mins`, `hours`, `days`, or `weeks`.
#'
#' @section Fields:
#' See [Terminator].
#'
#' @section Methods:
#' See [Terminator].
#'
#' @family Terminator
#' @examples
#' tm = TerminatorRuntime$new(max_time = 5, units = "secs")
#' @export
TerminatorRuntime = R6Class("TerminatorRuntime",
  inherit = Terminator,
  public = list(
    initialize = function(max_time, units) {
      super$initialize(settings = list(
        max_time = checkmate::assert_count(
          max_time,
          positive = TRUE,
          coerce = TRUE),
        units = checkmate::assert_choice(
          units,
          choices = c("secs", "mins", "hours", "days", "weeks"))))

      self$state = list(
        time_start = NULL,
        time_end = NULL,
        time_remaining = self$settings$max_time)
      self$terminated = FALSE
    },

    update_start = function(pe, measure = NA) {
      self$state$time_start = Sys.time()
      invisible(self)
    },

    update_end = function(pe, measure = NA) {
      self$state$time_end = Sys.time()
      dtime = difftime(
        time1 = self$state$time_end,
        time2 = self$state$time_start,
        units = self$settings$units)
      self$state$time_remaining = self$state$time_remaining - dtime
      self$terminated = self$state$time_remaining < 0
      invisible(self)
    }
  )
)
