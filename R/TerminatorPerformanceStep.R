#' @title TerminatorPerformanceStep
#'
#' @description
#' Terminator child class to terminate the sequential feature selection if the model performance does not improve to a specified threshold in the next step.
#'
#' @section Usage:
#'  ```
#' tm = TerminatorPerformanceStep$new(threshold)
#' ```
#' See [Terminator] for a description of the interface.
#'
#' @section Arguments:
#' * `threshold` (`numeric(1)``):
#'   The feature selection is terminated if the performance improvement between two steps is less than the threshold.
#'
#' @section Details:
#' `$new()` creates a new object of class [TerminatorPerformanceStep].
#'
#' The interface is described in [Terminator].
#'
#' @name TerminatorPerformanceStep
#' @family Terminator
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("holdout")
#' pe = PerformanceEvaluator$new(task, learner, resampling)
#' tm = TerminatorPerformanceStep$new(threshold = 0.01)
NULL

#' @export
#' @include Terminator.R
TerminatorPerformanceStep = R6Class("TerminatorPerformanceStep",
  inherit = Terminator,
  public = list(
    initialize = function(threshold) {
      super$initialize(
        settings = list(threshold = checkmate::assert_numeric(threshold)))

      self$terminated = FALSE
      self$state = list(step_performance = NA)
    },

    update_start = function(pe) {
      invisible(self)
    },
    update_end = function(pe) {
      bmr = pe$get_best()
      if (!is.na(self$state$step_performance)) {
        if (pe$task$measures[[1]]$minimize) {
          if (self$state$step_performance - bmr[[length(bmr)]]$performance <= self$settings$threshold) {
            self$terminated = TRUE
          }
        } else {
          if (bmr[[length(bmr)]]$performance - self$state$step_performance <= self$settings$threshold) {
            self$terminated = TRUE
          }
        }
      }
      self$state$step_performance = bmr[[length(bmr)]]$performance
      invisible(self)
    }
  )
)
