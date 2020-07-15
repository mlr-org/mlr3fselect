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
#' \item{`batch_size`}{`integer(1)`\cr
#' Maximum number of feature sets to try in a batch.}
#' }
#'
#' @source
#' \cite{bbotk}{bergstra_2012}
#'
#' @export
#' @template example
FSelectorRandomSearch = R6Class("FSelectorRandomSearch",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerRandomSearch$new()
      )
    }
  )
)

mlr_fselectors$add("random_search", FSelectorRandomSearch)
