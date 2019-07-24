#' @title FeatureSelectionExhaustive
#'
#' @description
#' FeatureSelection child class to conduct exhaustive search
#'
#' @section Usage:
#'  ```
#' fs = FeatureSelectionExhaustive$new()
#' ```
#' See [FeatureSelection] for a description of the interface.
#'
#' @section Arguments:
#' * `pe` (`[PerformanceEvaluator]`).
#' * `tm` (`[Terminator]`).
#' * `max_features` (`integer(1)`)
#'   Maximum number of features
#'
#' @section Details:
#' `$new()` creates a new object of class [FeatureSelectionExhaustive].
#' `$get_result()` Returns best feature combination.
#' The interface is described in [FeatureSelection].
#'
#' @name FeatureSelectionExhaustive
#' @family FeatureSelection
#' @examples
#' task = mlr3::mlr_tasks$get("pima")
#' task$select(c("age", "glucose", "insulin", "mass"))
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5L))
#' pe = PerformanceEvaluator$new(task = task, learner = learner, resampling = resampling)
#' tm = TerminatorRuntime$new(max_time = 20, units = "secs")
#' fs = FeatureSelectionExhaustive$new(pe = pe, tm = tm, max_features = 3)
#' fs$calculate()
#' fs$get_result()
NULL

#' @export
#' @include FeatureSelection.R

FeatureSelectionExhaustive = R6Class("FeatureSelectionExhaustive",
  inherit = FeatureSelection,
  public = list(
    initialize = function(pe, tm, measure, param_vals = list()) {
      super$initialize(id = "exhaustive_selection",
                       pe = pe,
                       tm = tm,
                       measure = measure,
                       param_vals = param_vals)

      self$state = private$generate_states(1)
    },

    get_result = function() {
      if (length(self$pe$bmr) > 1) {
        bmr = lapply(self$pe$bmr[1:length(self$pe$bmr)],
                     function(bmr) self$pe$bmr[[1]]$combine(bmr))
      } else {
        bmr = self$pe$bmr
      }
      bmr_best = bmr[[length(bmr)]]$get_best(self$pe$task$measures[[1L]]$id)
      list(
        features = bmr_best$task$feature_names,
        performance = bmr_best$aggregated)
    }
  ),
  private = list(
    calculate_step = function() {
      # Convert 0/1 states to feature names
      named_states = lapply(self$state, private$binary_to_features)

      # Evaluation
      private$eval_states_terminator(named_states)

      # Generate new states
      self$state = private$generate_states(
        min((sum(self$state[[1]]) + 1), self$param_set$values$max_features))
    },
    generate_states = function(feature_count) {
      combinations = combn(length(self$pe$task$feature_names), feature_count)
      self$state = lapply(seq_len(ncol(combinations)), function(j) {
        state = rep(0, length(self$pe$task$feature_names))
        state[combinations[, j]] = 1
        state
      })
    },
    eval_states_terminator = function(states) {
      self$tm$update_start(self$pe)
      self$pe$eval_states(states)
      self$tm$update_end(self$pe)

      # Side-effect stop
      if (!self$tm$terminated) {
        self$tm$terminated = (length(states[[1]]) == self$param_set$values$max_features)
      }
    }
  )
)
