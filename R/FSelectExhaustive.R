#' FSelectExhaustive Class
#'
#' @description
#' Class for exhaustive feature selection. All feature sets are searched.
#'
#' @section Parameters:
#' \describe{
#' \item{\code{max_features}}{\code{integer(1)} Maximum number of features. By default, number of features in [mlr3::Task].}}
#'
#' In order to support general termination criteria and parallelization, feature sets are evaluated in batches.
#' The size of the feature sets is increased by 1 in each batch.
#'
#' @export
#' @templateVar id exhaustive
#' @template example
FSelectExhaustive = R6Class("FSelectExhaustive",
  inherit = FSelect,
  public = list(
    #' @description
    #' Create new `FSelectExhaustive` object.
    #' @return `FSelectExhaustive`
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("max_features", lower = 1))
      )

      super$initialize(
        param_set = ps
      )
    }
  ),
  private = list(
    select_internal = function(instance) {

      pars = self$param_set$values
      if (is.null(pars$max_features)) pars$max_features = length(instance$task$feature_names)

      if (instance$n_batch + 1 > pars$max_features) {
        stop(terminated_error(instance))
      }

      combinations = combn(length(instance$task$feature_names), instance$n_batch + 1)
      states = t(sapply(seq_len(ncol(combinations)), function(j) {
        state = rep(0, length(instance$task$feature_names))
        state[combinations[, j]] = 1
        state
      }))

      instance$eval_batch(states)
    }
  )
)

mlr_fselectors$add("exhaustive", FSelectExhaustive)
