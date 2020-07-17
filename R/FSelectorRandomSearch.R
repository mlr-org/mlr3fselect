#' @title Feature Selection via Random Search
#'
#' @description
#' `FSelectorRandomSearch` class that implements a simple Random Search.
#'
#' In order to support general termination criteria and parallelization, we
#' evaluate feature sets in a batch-fashion of size `batch_size`. Larger batches
#' mean we can parallelize more, smaller batches imply a more fine-grained
#' checking of termination criteria.
#'
#' @templateVar id random_search
#' @template section_dictionary_fselectors
#'
#' @section Parameters:
#' \describe{
#' \item{`max_features`}{`integer(1)`\cr
#' Maximum number of features. By default, number of features in [mlr3::Task].}
#' \item{`batch_size`}{`integer(1)`\cr
#' Maximum number of feature sets to try in a batch.}
#' \item{`prob`}{`double(1)`\cr
#' Probability of choosing a feature.}
#' }
#'
#' @source
#' \cite{bbotk}{bergstra_2012}
#'
#' @export
#' @template example
FSelectorRandomSearch = R6Class("FSelectorRandomSearch",
  inherit = FSelector,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("max_features", lower = 1),
        ParamInt$new("batch_size", default = 10, lower = 1),
        ParamDbl$new("prob", default = 0.5, lower = 0, upper = 1))
      )

      ps$values = list(batch_size = 10L, prob = 0.5)

      super$initialize(param_set = ps, properties = "single-crit")
    }
  ),

  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      feature_names = inst$archive$cols_x

      if (is.null(pars$max_features)) {
        pars$max_features = length(feature_names)
      }
      repeat({
        states =
          map_dtr(seq_len(pars$batch_size), function(i) {
            x = Inf
            while (sum(x) > pars$max_features | sum(x) == 0) {
              x = rbinom(length(feature_names), 1, pars$prob)
            }
            set_names(as.list(as.logical(x)), feature_names)
          })

        inst$eval_batch(states)
      })
    }
  )
)

mlr_fselectors$add("random_search", FSelectorRandomSearch)