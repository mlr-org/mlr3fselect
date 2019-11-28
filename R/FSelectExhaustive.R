#' FSelectExhaustive Class
#'
#' @description
#' Class for exhaustive feature selection.
#'
#' @section Parameters:
#' \describe{
#' \item{\code{max_features}}{\code{integer(1)} Maximum number of features. By default, number of features in [mlr3::Task].}}
#'
#' The feature combinations are evaluated in batches.
#' Each batch increases the number features.
#'
#' @export
FSelectExhaustive = R6Class("FSelectExhaustive",
  inherit = FSelect,
  public = list(
    #' @description
    #' Create new `FSelectExhaustive` object.
    #' @return A `FSelectExhaustive` object.
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

      if (length(instance$bmr$rr_data$batch_n) == 0) f_nr = 1 else f_nr = instance$bmr$rr_data$batch_n[length(instance$bmr$rr_data$batch_n)] + 1

      if (f_nr > pars$max_features) {
        stop(terminated_error(instance))
      }

      combinations = combn(length(instance$task$feature_names), f_nr)
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
