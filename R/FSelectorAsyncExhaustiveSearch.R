#' @title Feature Selection with Asynchronous Exhaustive Search
#'
#' @include FSelectorAsync.R
#' @name mlr_fselectors_async_exhaustive_search
#'
#' @description
#' Feature Selection using the Asynchronous Exhaustive Search Algorithm.
#' Exhaustive Search generates all possible feature sets.
#' The feature sets are evaluated asynchronously.
#'
#' @details
#' The feature selection terminates itself when all feature sets are evaluated.
#' It is not necessary to set a termination criterion.
#'
#' @templateVar id async_exhaustive_search
#' @template section_dictionary_fselectors
#'
#' @section Control Parameters:
#' \describe{
#' \item{`max_features`}{`integer(1)`\cr
#'   Maximum number of features.
#'   By default, number of features in [mlr3::Task].}
#' }
#'
#' @family FSelectorAsync
#' @export
#' @template example
FSelectorAsyncExhaustiveSearch = R6Class("FSelectorAsyncExhaustiveSearch",
  inherit = FSelectorAsync,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        max_features = p_int(lower = 1L)
      )

      super$initialize(
        id = "async_exhaustive_search",
        param_set = ps,
        properties = c("single-crit", "multi-crit", "async"),
        packages = "rush",
        label = "Asynchronous Exhaustive Search",
        man = "mlr3fselect::mlr_fselectors_async_exhaustive_search")
    },

    #' @description
    #' Starts the asynchronous optimization.
    #'
    #' @param inst ([FSelectInstanceAsyncSingleCrit] | [FSelectInstanceAsyncMultiCrit]).
    #' @return [data.table::data.table].
    optimize = function(inst) {
      pars = self$param_set$values
      feature_names = inst$archive$cols_x
      n_features = length(feature_names)

      fun = function(i, state) {
        state[i] = TRUE
        as.list(state)
      }

      states = set_col_names(rbindlist(unlist(map(seq(pars$max_features %??% n_features), function(n) {
        combn(n_features, n, fun, simplify = FALSE, state = logical(n_features))
      }), recursive = FALSE)), feature_names)

      optimize_async_default(inst, self, states)
    }
  ),

  private = list(
    .optimize = function(inst) {
      # evaluate feature sets
      get_private(inst)$.eval_queue()
    }
  )
)

mlr_fselectors$add("async_exhaustive_search", FSelectorAsyncExhaustiveSearch)
