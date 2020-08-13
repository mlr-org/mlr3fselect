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
#' }
#'
#' @source
#' \cite{mlr3fselect}{bergstra_2012}
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
        ParamInt$new("batch_size", default = 10, lower = 1))
      )

      ps$values = list(batch_size = 10L)

      super$initialize(param_set = ps, properties = c("single-crit",
        "multi-crit"))
    }
  ),

  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      feature_names = inst$archive$cols_x
      max_features = pars$max_features %??% length(feature_names)

      repeat {
        X = t(replicate(pars$batch_size, {
          n = sample.int(max_features, 1L)
          x = sample.int(length(feature_names), n)
          replace(logical(length(feature_names)), x, TRUE)
        }))
        colnames(X) = feature_names
        inst$eval_batch(as.data.table(X))
      }
    }
  )
)

mlr_fselectors$add("random_search", FSelectorRandomSearch)
