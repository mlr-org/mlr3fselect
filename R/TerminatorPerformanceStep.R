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
#' * `pe` (`[PerformanceEvaluator]`)
#' * `threshold` (`numeric(1)``):
#' The feature selection is terminated if the performance improvement between two steps is less than the threshold.
#' * `max_features` (`integer(1)`)
#' Maximum number of features
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
#' t = TerminatorPerformanceStep$new(pe, threshold = 0.01, max_features = 2)
NULL

#' @export
#' @include Terminator.R
TerminatorPerformanceStep = R6Class("TerminatorPerformanceStep",
  inherit = Terminator,
  public = list(
    threshold = NULL,
    max_features = NULL,

    initialize = function(pe, threshold, max_features = NA) {
      super$initialize(settings = list(threshold = checkmate::assert_numeric(threshold),
                                       max_features = checkmate::assert_numeric(max_features,
                                                                                lower = 1,
                                                                                upper = length(pe$task$feature_names))))
      self$terminated = FALSE
      self$state = list(step_performance = NA)

      if(is.na(self$settings$max_features)) {
        self$settings$max_features <- length(pe$task$feature_names)
      }
    },

    update_start = function(pe) {
      invisible(self)
    },
    update_end = function(pe) {
      bmr = pe$get_best()
      # Stop if max_features is reached or all features are included
      if(length(pe$bmr) == self$settings$max_features) {
        self$terminated = TRUE
      }

      # Stop if threshold is reached
      if(!is.na(self$state$step_performance)) {
        if(pe$task$measures[[1]]$minimize) {
          if(self$state$step_performance - bmr[[length(bmr)]]$performance <= self$settings$threshold) {
            self$terminated = TRUE
          }
        } else {
          if(bmr[[length(bmr)]]$performance - self$state$step_performance <= self$settings$threshold) {
            self$terminated = TRUE
          }
        }
      }
      self$state$step_performance = bmr[[length(bmr)]]$performance
      invisible(self)
    }
  )
)
