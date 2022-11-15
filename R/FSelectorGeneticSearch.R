#' @title Feature Selection with Genetic Search
#'
#' @include mlr_fselectors.R
#' @name mlr_fselectors_genetic_search
#'
#' @description
#' Feature selection using the Genetic Algorithm from the package \CRANpkg{genalg}.
#'
#' @templateVar id genetic_search
#' @template section_dictionary_fselectors
#'
#' @section Control Parameters:
#' For the meaning of the control parameters, see [genalg::rbga.bin()].
#' [genalg::rbga.bin()] internally terminates after `iters` iteration.
#' We set `ìters = 100000`  to allow the termination via our terminators.
#' If more iterations are needed, set `ìters` to a higher value in the parameter set.
#'
#' @family FSelector
#' @export
#' @template example
FSelectorGeneticSearch = R6Class("FSelectorGeneticSearch",
  inherit = FSelector,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        suggestion     = p_uty(),
        popSize        = p_int(lower = 5L, default = 200L),
        mutationChance = p_dbl(lower = 0, upper = 1),
        elitism        = p_int(lower = 1L),
        zeroToOneRatio = p_int(lower = 1, default = 10L),
        iters          = p_int(lower = 1, default = 100000L)
      )
      ps$values$iters = 100000L

      super$initialize(
        id = "genetic_search",
        param_set = ps,
        properties = "single-crit",
        label = "Genetic Search",
        man = "mlr3fselect::mlr_fselectors_genetic_search"
      )
    }
  ),
  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      if (is.null(pars$mutationChance)) pars$mutationChance = NA
      if (is.null(pars$elitism)) pars$elitism = NA
      n = inst$objective$domain$length

      mlr3misc::invoke(genalg::rbga.bin, size = n, evalFunc = inst$objective_function, .args = pars)
    }
  )
)

mlr_fselectors$add("genetic_search", FSelectorGeneticSearch)
