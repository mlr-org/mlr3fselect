#' @title Feature Selection with Exhaustive Search
#'
#' @include mlr_fselectors.R
#' @name mlr_fselectors_exhaustive_search
#'
#' @description
#' Feature Selection using the Exhaustive Search Algorithm.
#' Exhaustive Search generates all possible feature sets.
#'
#' @details
#' The feature selection terminates itself when all feature sets are evaluated.
#' It is not necessary to set a termination criterion.
#'
#' @templateVar id exhaustive_search
#' @template section_dictionary_fselectors
#'
#' @section Control Parameters:
#' \describe{
#' \item{`max_features`}{`integer(1)`\cr
#'   Maximum number of features.
#'   By default, number of features in [mlr3::Task].}
#' }
#'
#' @family FSelector
#' @export
#' @template example
FSelectorExhaustiveSearch = R6Class("FSelectorExhaustiveSearch",
  inherit = FSelector,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        max_features = p_int(lower = 1L),
        batch_size = p_int(lower = 1L, default = 10L, tags = "required")
      )
      ps$values = list(batch_size = 10L)

      super$initialize(
        id = "exhaustive_search",
        param_set = ps,
        properties = c("single-crit", "multi-crit"),
        label = "Exhaustive Search",
        man = "mlr3fselect::mlr_fselectors_exhaustive_search")
    }
  ),
  private = list(
    .optimize = function(inst) {
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

      chunks = split(seq_row(states), ceiling(seq_along(seq_row(states)) / pars$batch_size))
      walk(chunks, function(row_ids) inst$eval_batch(states[row_ids]))
    }
  )
)

mlr_fselectors$add("exhaustive_search", FSelectorExhaustiveSearch)
