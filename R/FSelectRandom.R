#' FSelectRandom Class
#'
#' @description
#' Class for random feature selection. Feature sets are randomly drawn.
#'
#' @section Parameters:
#' \describe{
#' \item{\code{max_features}}{\code{integer(1)} Maximum number of features. By default, number of features in [mlr3::Task].}
#' \item{\code{batch_size}}{\code{integer(1)} Maximum number of feature sets to try in a batch.}
#' \item{\code{prob}}{\code{double(1)} Probability of choosing a feature.}}
#'
#' In order to support general termination criteria and parallelization,
#' feature sets are evaluated in a batch-fashion of size `batch_size`.
#' Larger batches mean more is parallelized, smaller batches imply a more fine-grained checking
#' of termination criteria.
#'
#' @export
#' @templateVar id random
#' @template example
FSelectRandom = R6Class("FSelectRandom",
  inherit = FSelect,
  public = list(
    #' @description
    #' Create new `FSelectRandom` object.
    #' @return `FSelectRandom`
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("max_features", lower = 1),
        ParamInt$new("batch_size", default = 10, lower = 1),
        ParamDbl$new("prob", default = 0.5, lower = 0, upper = 1))
      )

      super$initialize(
        param_set = ps
      )
      if (is.null(self$param_set$values$batch_size)) {
        self$param_set$values = insert_named(self$param_set$values, list(batch_size = 10))
      }
      if (is.null(self$param_set$values$prob)) {
        self$param_set$values = insert_named(self$param_set$values, list(prob = 0.5))
      }
    }
  ),
  private = list(
    select_internal = function(instance) {
      pars = self$param_set$values
      if (is.null(pars$max_features)) pars$max_features = length(instance$task$feature_names)

      states = t(sapply(seq_len(pars$batch_size), function(i) {
        x = Inf
        while (sum(x) > pars$max_features | sum(x) == 0) {
          x = rbinom(length(instance$task$feature_names), 1, pars$prob)
        }
        return(x)
      }))

      instance$eval_batch(states)
    }
  )
)

mlr_fselectors$add("random", FSelectRandom)
