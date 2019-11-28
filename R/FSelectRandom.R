#' FSelectRandom Class
#'
#' @description
#' Class for random feature selection.
#'
#' @section Parameters:
#' \describe{
#' \item{\code{max_features}}{\code{integer(1)} Maximum number of features. By default, number of features in [mlr3::Task].}
#' \item{\code{batch_size}}{\code{integer(1)} Maximum number of feature combinations to try in a batch.}}
#'
#' In order to support general termination criteria and parallelization,
#' feature combinations are evaluated in a batch-fashion of size `batch_size`.
#' Larger batches mean more is parallelized, smaller batches imply a more fine-grained checking
#' of termination criteria.
#'
#' @export
FSelectRandom = R6Class("FSelectRandom",
  inherit = FSelect,
  public = list(
    #' @description
    #' Create new `FSelectRandom` object.
    #' @return A `FSelectRandom` object.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("max_features", lower = 1),
        ParamInt$new("batch_size", default = 10, lower = 1))
      )

      super$initialize(
        param_set = ps
      )
    }
  ),
  private = list(
    select_internal = function(instance) {
      pars = self$param_set$values

      states = t(sapply(seq_len(pars$batch_size), function(i) {
        x = Inf
        while (sum(x) > pars$max_features | sum(x) == 0) {
          x = rbinom(length(instance$task$feature_names), 1, 0.5)
        }
        return(x)
      }))

      instance$eval_batch(states)
    },

    set_defaults = function(instance) {
      if (is.null(self$param_set$values$max_features)) self$param_set$values$max_features = length(instance$task$feature_names)
      if (is.null(self$param_set$values$batch_size)) self$param_set$values$batch_size = self$param_set$default[["batch_size"]]
    }
  )
)

mlr_fselectors$add("random", FSelectRandom)
