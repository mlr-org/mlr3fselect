#' @title TerminatorRuntime Class
#'
#' @description
#' Terminator child class to terminate the feature selection after a specific time. Note that the runtime is checked after each step and therefore it could happen that the final runtime is longer than the specified one. Time is measured for everything that happens between update_start and update_end.
#' @section Usage:
#'  ```
#' tm = TerminatorRuntime$new(max_time, time_unit)
#' ```
#' See [Terminator] for a description of the interface.
#'
#' @section Arguments:
#' * `max_time` (integer(1)):
#'   Maximal amount of time measures in `units`.
#' * `units` (character(1)):
#'   Unit used for measuring time. Possible choices are "secs", "mins", "hours", "days", and "weeks" that
#'   are directly passed to `difftime()`.
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorRuntime].
#'
#' The interface is described in [Terminator].
#'
#' @name TerminatorRuntime
#' @family Terminator
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("holdout")
#' pe = PerformanceEvaluator$new(task, learner, resampling)
#' tm = TerminatorRuntime$new(max_time = 5, units = "secs")
NULL

#' @export
#' @include Terminator.R
TerminatorRuntime = R6Class("TerminatorRuntime",
  inherit = Terminator,
  public = list(
    initialize = function(max_time, units) {
      super$initialize(settings = list(max_time = checkmate::assert_count(max_time, positive = TRUE, coerce = TRUE),
                                       units = checkmate::assert_choice(units, choices = c("secs", "mins", "hours", "days", "weeks"))))

      self$state = list(time_start = NULL, time_end = NULL, time_remaining = self$settings$max_time)
      self$terminated = FALSE
    },

    update_start = function(pe) {
      self$state$time_start = Sys.time()
      invisible(self)
    },

    update_end = function(pe) {
      self$state$time_end = Sys.time()
      dtime = difftime(time1 = self$state$time_end, time2 = self$state$time_start, units = self$settings$units)
      self$state$time_remaining = self$state$time_remaining - dtime
      self$terminated = self$state$time_remaining < 0
      invisible(self)
    }
  )
)
