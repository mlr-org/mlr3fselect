#' @title Feature Selection with Asynchronous Random Search
#'
#' @include mlr_fselectors.R
#' @name mlr_fselectors_async_random_search
#'
#' @description
#' Feature selection using Asynchronous Random Search Algorithm.
#'
#' @templateVar id async_random_search
#' @template section_dictionary_fselectors
#'
#' @section Control Parameters:
#' \describe{
#' \item{`max_features`}{`integer(1)`\cr
#' Maximum number of features.
#' By default, number of features in [mlr3::Task].}
#' }
#'
#' @source
#' `r format_bib("bergstra_2012")`
#'
#' @family FSelectorAsync
#' @export
FSelectorAsyncRandomSearch = R6Class("FSelectorAsyncRandomSearch",
  inherit = FSelectorAsync,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        max_features = p_int(lower = 1L)
      )

      super$initialize(
        id = "async_random_search",
        param_set = ps,
        properties = c("single-crit", "multi-crit"),
        label = "Asynchronous Random Search",
        man = "mlr3fselect::mlr_fselectors_async_random_search"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      feature_names = inst$archive$cols_x
      max_features = pars$max_features %??% length(feature_names)

      # usually the queue is empty but callbacks might have added points
      get_private(inst)$.eval_queue()

      while (!inst$is_terminated) {
        # sample new points
        n = sample.int(max_features, 1L)
        x = sample.int(length(feature_names), n)
        xs = as.list(set_names(replace(logical(length(feature_names)), x, TRUE), feature_names))
        # evaluate
        get_private(inst)$.eval_point(xs)
      }
    }
  )
)

mlr_fselectors$add("async_random_search", FSelectorAsyncRandomSearch)
