#' @title FeatureSelectionSequential
#'
#' @format [R6::R6Class] inheriting from [FeatureSelection].
#' @include FeatureSelection.R
#'
#' @description
#' Sequential feature selection wrapper.
#'
#' @section Construction:
#'  ```
#' fs = FeatureSelectionSequential$new(pe, tm, measure, param_vals)
#' ```
#'
#' For arguments, see [FeatureSelection].
#'
#' @section Fields:
#' See [FeatureSelection].
#'
#' @section Methods:
#' See [FeatureSelection] and additionally:
#'
#' * `$get_result()`\cr Returns best feature set.
#'
#' * `$get_path()`\cr Returns each step.
#'
#' @section Parameter Values:
#' See [FeatureSelection] and additionally:
#'
#' * `strategy` :: `character(1)`\cr `fsf` for forward or `fsb` for backward feature selection.
#'
#' @name FeatureSelectionSequential
#' @family FeatureSelection
#' @examples
#' task = mlr3::mlr_tasks$get("pima")
#' measure = mlr3::mlr_measures$get(c("classif.acc"))
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5L))
#' resampling$instantiate(task)
#' pe = PerformanceEvaluator$new(task, learner, resampling)
#' tm = TerminatorPerformanceStep$new(threshold = 0.01)
#' fs = FeatureSelectionSequential$new(pe = pe, tm = tm, measure,
#'                                     param_vals = list(max_features = 4))
#' fs$calculate()
#' fs$get_result()
#' @export
FeatureSelectionSequential = R6Class("FeatureSelectionSequential",
  inherit = FeatureSelection,
  public = list(
    initialize = function(pe, tm, measure, param_vals = list()) {
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
        bmr = bmr$best(self$measure)
        list(
          features = bmr$task$feature_names,
          performance = bmr$aggregate(self$measure))
      })
    }
  ),
  private = list(
    calculate_step = function() {
      # Convert 0/1 states to feature names
      named_states = lapply(self$state, function(state) {
        self$pe$task$feature_names[as.logical(state)]})

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
