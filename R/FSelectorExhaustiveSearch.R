#' @title Feature Selection via Exhaustive Search
#'
#' @name mlr_fselectors_exhaustive_search
#'
#' @description
#' Exhaustive search generates all possible feature sets.
#'
#' The feature selection terminates itself when all feature sets are evaluated.
#' It is not necessary to set a termination criterion.
#'
#' @templateVar id exhaustive_search
#' @template section_dictionary_fselectors
#'
#' @section Parameters:
#' \describe{
#' \item{`max_features`}{`integer(1)`\cr
#' Maximum number of features. By default, number of features in [mlr3::Task].}
#' }
#'
#' @export
#' @template example
FSelectorExhaustiveSearch = R6Class("FSelectorExhaustiveSearch",
  inherit = FSelector,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        max_features = p_int(lower = 1)
      )

      super$initialize(
        param_set = ps,
        properties = c("single-crit", "multi-crit"),
        man = "mlr3fselect::mlr_fselectors_exhaustive_search")
    }
  ),
  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      feature_names = inst$archive$cols_x
      archive = inst$archive

      if (is.null(pars$max_features)) {
        pars$max_features = length(feature_names)
      }

      repeat({
        combinations = combn(length(feature_names),
          archive$n_batch + 1)
        states = map_dtr(seq_len(ncol(combinations)), function(j) {
          state = rep(FALSE, length(feature_names))
          state[combinations[, j]] = TRUE
          set_names(as.list(state), feature_names)
        })
        inst$eval_batch(states)

        if (archive$n_batch == pars$max_features) break
      })
    }
  )
)

mlr_fselectors$add("exhaustive_search", FSelectorExhaustiveSearch)
