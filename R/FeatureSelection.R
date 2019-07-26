#' @title Feature Selection Wrapper Base Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description This is the base class for feature selection wrappers.
#'
#' @section Construction:
#' ```
#' fs = FeatureSelectionr$new(id, pe, tm, measure, param_set, param_vals)
#' ```
#'
#' * `id` :: `character(1)`\cr Identifier for the wrapper
#'
#' * `pe` :: [PerformanceEvaluator]\cr
#'
#' * `tm` :: [Terminator]\cr
#'
#' * `param_set` :: [paradox::ParamSet]\cr Set of hyperparameters.
#'
#' * `param_vals` :: named `list()`\cr Named list of hyperparameter settings.
#'
#' @section Fields:
#' * `id` :: `character(1)`\cr Stores the identifier of of the wrapper.
#'
#' * `pe` :: [PerformanceEvaluator]\cr The current state of the [PerformanceEvaluator].
#'
#' * `tm` :: [Terminator]\cr The current state of the [Terminator].
#'
#' * `measure` :: [mlr3::Measure]\cr Stores the measure.
#'
#' * `param_set` :: [paradox::ParamSet]\cr Description of available hyperparameters and hyperparameter settings.
#'
#' @section Methods:
#' * `$calculate()`\cr Performs the feature selection until the budget of [Terminator] is exhausted.
#'
#' @name FeatureSelection
#' @family FeatureSelection
#' @export
FeatureSelection = R6Class("FeatureSelection",
  public = list(
    id = NULL,
    pe = NULL,
    tm = NULL,
    measure = NULL,
    param_set = NULL,
    state = NULL,

    initialize = function(id, pe, tm, measure, param_set = ParamSet$new(),
      param_vals = list()) {

      self$id = checkmate::assert_string(id)
      self$pe = checkmate::assert_r6(pe, "PerformanceEvaluator")
      self$tm = checkmate::assert_r6(tm, "Terminator")
      self$measure = checkmate::assert_r6(measure, "Measure")
      checkmate::assert_list(param_vals)

      param_set$add(ParamSet$new(list(
        ParamInt$new("max_features",
          lower = 1,
          upper = length(pe$task$feature_names),
          default = length(pe$task$feature_names),
          tags = "generic")))
      )
      self$param_set = assert_param_set(param_set)

      # Set values to default if missing
      if (is.null(param_vals$max_features)) {
        param_vals$max_features = self$param_set$default[["max_features"]]
      }

      self$param_set$values = param_vals
    },
    calculate = function() {
      while (!self$tm$terminated) {
        private$calculate_step()
      }
    }
  ),
  private = list(
    binary_to_features = function(binary_features) {
      task$feature_names[as.logical(binary_features)]
    },
    eval_states_terminator = function(states) {
      self$tm$update_start(self$pe, self$measure)
      self$pe$eval_states(states)
      self$tm$update_end(self$pe, self$measure)
    }
  )
)
