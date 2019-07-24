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
    initialize = function(pe, tm, measure, param_vals) {
      super$initialize(id = "sequential_selection",
                       pe = pe,
                       tm = tm,
                       measure = measure,
                       param_set = ParamSet$new(list(
                         ParamFct$new("strategy", levels = c("fsf", "fsb"), default = "fsf"))),
                       param_vals = param_vals)

      # Set values to default if missing
      if (is.null(self$param_set$values$strategy)) {
        self$param_set$values$strategy = self$param_set$default[["strategy"]]
      }

      if (self$param_set$values$strategy == "fsf") {
        self$state = private$generate_states(rep(0, length(pe$task$feature_names)))
      } else if (self$param_set$values$strategy == "fsb") {
        self$state = rep(list(rep(1, length(pe$task$feature_names))), length(pe$task$feature_names))
      }
    },

    get_result = function() {
      bmr_best = self$pe$bmr[[length(self$pe$bmr)]]$best(self$measure)
      list(
        features = bmr_best$task$feature_names,
        performance = bmr_best$aggregate(self$measure))
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
      bmr = self$pe$bmr[[length(self$pe$bmr)]]
      bmr_best = bmr$best(self$measure)
      best_state = as.numeric(Reduce("|", lapply(bmr_best$task$feature_names, function(x) x == self$pe$task$feature_names)))

      # Generate new states
      self$state = private$generate_states(best_state)
    },
    generate_states = function(state) {
      x = ifelse(self$param_set$values$strategy == "fsf", 0, 1)
      y = ifelse(self$param_set$values$strategy == "fsf", 1, 0)
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
      self$tm$update_start(self$pe, self$measure)
      self$pe$eval_states(states)
      self$tm$update_end(self$pe, self$measure)

      # Side-effect stop
      if (!self$tm$terminated) {
        if (self$param_set$values$strategy == "fsf") {
          self$tm$terminated = (length(states[[1]]) == self$param_set$values$max_features)
        } else if (self$param_set$values$strategy == "fsb") {
          self$tm$terminated = (length(states[[1]]) == 1)
        }
      }
    }
  )
)
