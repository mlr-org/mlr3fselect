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
#' \item{`iters`}{`integer(1)`}
#' }
#'
#' For the meaning of the control parameters, see [genalg::rbga.bin()].
#' [genalg::rbga.bin()] internally terminates after `iters` iteration. We set
#' `ìters = 100000`  to allow the termination via our terminators. If more
#' iterations are needed, set `ìters` to a higher value in the parameter set.
#'
#' @export
#' @examples 
#' library(mlr3)
#'
#' terminator = trm("evals", n_evals = 5)
#'
#' instance = FSelectInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = terminator
#' )
#'
#' fselector = fs("genetic_search", popSize = 10L)
#' \donttest{
#' # Modifies the instance by reference
#' fselector$optimize(instance)
#'
#' # Returns best scoring evaluation
#' instance$result
#'
#' # Allows access of data.table of full path of all evaluations
#' instance$archive$data()}
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
        ParamInt$new("zeroToOneRatio", lower = 1, default = 10L),
        ParamInt$new("iters", lower = 1, default = 100000L)
      ))
      ps$values$iters = 100000L

      super$initialize(param_set = ps, properties = "single-crit")
    }
  ),
  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      if (is.null(pars$mutationChance)) pars$mutationChance = NA
      if (is.null(pars$elitism)) pars$elitism = NA
      n = inst$objective$domain$length

      mlr3misc::invoke(genalg::rbga.bin, size = n,
        evalFunc = inst$objective_function, .args = pars)
    }
  )
)

mlr_fselectors$add("genetic_search", FSelectorGeneticSearch)