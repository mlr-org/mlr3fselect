#' FSelectSequential Class
#'
#' @description
#' Class for sequential feature selection.
#'
#' \describe{
#' \item{\code{max_features}}{\code{integer(1)} Maximum number of features. By default, number of features in [mlr3::Task].}
#' \item{\code{strategy}}{\code{character(1)} For forward feature selection `fsf`, for backward feature selection `fsb`}}
#'
#' The feature combinations are evaluated in batches.
#' Each batch is one step in the sequential feature selection.
#' In order to diplay the selected features of each step,
#' use the `best_by_batch` method of the [FSelectInstance].
#'
#' @export
FSelectSequential = R6Class("FSelectSequential",
  inherit = FSelect,
  public = list(
    #' @description
    #' Create new `FSelectSequential` object.
    #' @return A `FSelectSequential` object.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("max_features", lower = 1),
        ParamFct$new("strategy", levels = c("fsf", "fsb"), default = "fsf"))
      )

      super$initialize(
        param_set = ps
      )
      if (is.null(self$param_set$values$strategy)) {
        self$param_set$values = insert_named(self$param_set$values, list(strategy = "fsf"))
      }
    }
  ),
  private = list(
    select_internal = function(instance) {
      pars = self$param_set$values
      if (is.null(pars$max_features)) pars$max_features = length(instance$task$feature_names)

      # Initialize states for first batch
      if (instance$n_batch == 0) {
        if (self$param_set$values$strategy == "fsf") {
          states = diag(1, length(instance$task$feature_names), length(instance$task$feature_names))
        } else {
          combinations = combn(length(instance$task$feature_names), pars$max_features)
          states = t(sapply(seq_len(ncol(combinations)), function(j) {
            state = rep(0, length(instance$task$feature_names))
            state[combinations[, j]] = 1
            state
          }))
        }
      } else {
        if (instance$n_batch == pars$max_features) {
          stop(terminated_error(instance))
        }

        # Query bmr for best feature subset of last batch
        rr = instance$best(m = instance$n_batch)
        feat = instance$bmr$rr_data[rr$uhash, on = "uhash"]$feat[[1]]
        best_state = as.numeric(instance$task$feature_names %in% feat)

        # Generate new states based on best feature combination
        x = ifelse(pars$strategy == "fsf", 0, 1)
        y = ifelse(pars$strategy == "fsf", 1, 0)
        z = if (pars$strategy == "fsf") !as.logical(best_state) else as.logical(best_state)
        states = t(sapply(seq_along(best_state)[z], function(i) {
          if (best_state[i] == x) {
            new_state = best_state
            new_state[i] = y
            new_state
          }
        }))
      }
      instance$eval_batch(states)
    }
  )
)

mlr_fselectors$add("sequential", FSelectSequential)
