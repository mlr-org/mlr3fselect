#' @title FeatureSelectionSequential
#'
#' @description
#' FeatureSelection child class to conduct sequential search.
#'
#' @section Usage:
#'  ```
#' fs = FeatureSelectionSequential$new()
#' ```
#' See [FeatureSelection] for a description of the interface.
#'
#' @section Arguments:
#' * `pe` (`[PerformanceEvaluator]`).
#' * `tm` (`[Terminator]`).
#' * `max_features` (`integer(1)`)
#'   Maximum number of features
#' * `strategy` (`character(1)`).
#'   Forward selection `fsf` or backward selection `fsb`.
#'
#' @section Details:
#' `$new()` creates a new object of class [FeatureSelectionSequential].
#' `$get_result()` Returns selected features in each step.
#' The interface is described in [FeatureSelection].
#'
#' Each step is possibly executed in parallel via [mlr3::benchmark()]
#'
#' @name FeatureSelectionSequential
#' @family FeatureSelection
#' @examples
#' task = mlr3::mlr_tasks$get("pima")
#' measures = mlr3::mlr_measures$mget(c("classif.acc"))
#' task$measures = measures
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5L))
#' pe = PerformanceEvaluator$new(task, learner, resampling)
#' tm = TerminatorPerformanceStep$new(threshold = 0.01)
#' fs = FeatureSelectionSequential$new(pe, tm)
#' fs$calculate()
#' fs$get_result()
NULL

#' @export
#' @include FeatureSelection.R

FeatureSelectionSequential = R6Class("FeatureSelectionSequential",
  inherit = FeatureSelection,
  public = list(
    initialize = function(pe, tm, max_features = NA, strategy = "fsf") {
      if (is.na(max_features)) {
        max_features = length(pe$task$feature_names)
      }

      super$initialize(id = "sequential_selection", pe = pe, tm = tm,
        settings = list(
          max_features = checkmate::assert_numeric(
            max_features,
            lower = 1,
            upper = length(pe$task$feature_names)),
          strategy = checkmate::assert_string(
            strategy,
            pattern = "(^fsf$|^fsb$)")))

      if (strategy == "fsf") {
        self$state = private$generate_states(rep(0, length(pe$task$feature_names)))
      } else if (strategy == "fsb") {
        self$state = rep(list(rep(1, length(pe$task$feature_names))), length(pe$task$feature_names))
      }
    },

    get_result = function() {
      bmr = self$pe$bmr[[length(self$pe$bmr)]]$get_best(self$pe$task$measures[[1L]]$id)
      list(
        features = bmr$task$feature_names,
        performance = bmr$aggregated)
    },
    get_path = function() {
      lapply(self$pe$bmr, function(bmr) {
        bmr = bmr$get_best(self$pe$task$measures[[1L]]$id)
        list(
          features = bmr$task$feature_names,
          performance = bmr$aggregated)
      })
    }
  ),
  private = list(
    calculate_step = function() {

      # Convert 0/1 states to feature names
      named_states = lapply(self$state, private$binary_to_features)

      # Evaluation
      private$eval_states_terminator(named_states)

      # Select best state
      bmr = self$pe$get_best()
      features = bmr[[length(bmr)]]$features
      best_state = as.numeric(Reduce("|", lapply(features, function(x) x == self$pe$task$feature_names)))

      # Generate new states
      self$state = private$generate_states(best_state)
    },
    generate_states = function(state) {
      x = ifelse(self$settings$strategy == "fsf", 0, 1)
      y = ifelse(self$settings$strategy == "fsf", 1, 0)
      new_states = list()
      for (i in seq_along(state)) {
        if (state[i] == x) {
          changed_state = state
          changed_state[i] = y
          new_states[[length(new_states) + 1]] = changed_state
        }
      }
      new_states
    },
    eval_states_terminator = function(states) {
      self$tm$update_start(self$pe)
      self$pe$eval_states(states)
      self$tm$update_end(self$pe)

      # Side-effect stop
      if (!self$tm$terminated) {
        if (self$settings$strategy == "fsf") {
          self$tm$terminated = (length(states[[1]]) == self$settings$max_features)
        } else if (self$settings$strategy == "fsb") {
          self$tm$terminated = (length(states[[1]]) == 1)
        }
      }
    }
  )
)
