#' @title Feature Selection via Genetic Search
#'
#' @description
#' `FSelectorGeneticSearch` class that implements an Genetic Search. Calls
#' [genalg::rbga.bin()] from package \CRANpkg{genalg}.
#'
#' @templateVar id genetic_search
#' @template section_dictionary_fselectors
#'
#' @section Parameters:
#' \describe{
#' \item{`suggestions`}{`list()`}
#' \item{`popSize`}{`integer(1)`}
#' \item{`mutationChance`}{`numeric(1)`}
#' \item{`elitism`}{`integer(1)`}
#' \item{`zeroToOneRatio`}{`integer(1)`}
#' }
#'
#' For the meaning of the control parameters, see [genalg::rbga.bin()]. Note
#' that we have removed all control parameters which refer to the termination of
#' the algorithm and where our terminators allow to obtain the same behavior.
#'
#' @export
#' @template example
FSelectorGeneticSearch = R6Class("FSelectorGeneticSearch",
  inherit = FSelector,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamUty$new("suggestions"),
        ParamInt$new("popSize", lower = 5L, default = 200L),
        ParamDbl$new("mutationChance", lower = 0, upper = 1),
        ParamInt$new("elitism", lower = 1L),
        ParamInt$new("zeroToOneRatio", lower = 1, default = 10L)
      ))

      super$initialize(param_set = ps, properties = "single-crit")
    }
  ),
  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      if (is.null(pars$mutationChance)) pars$mutationChance = NA
      if (is.null(pars$elitism)) pars$elitism = NA
      n = inst$objective$domain$length

      # FIXME: iters = .Machine$integer.max crashes since genalg
      # generates a vector of 1:iters
      mlr3misc::invoke(genalg::rbga.bin, size = n, iters = 100000,
        evalFunc = inst$objective_function, .args = pars)
    }
  )
)

mlr_fselectors$add("genetic_search", FSelectorGeneticSearch)
