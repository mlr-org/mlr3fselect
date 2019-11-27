#' FSelectEvolutionary Class
#'
#' @description
#' Class for evolutionary feature selection calling [ecr::ecr()] from package \CRANpkg{ecr}.
#'
#' @section Parameters:
#' \describe{
#' \item{\code{mu}}{\code{integer(1)}}
#' \item{\code{lambda}}{\code{integer(1)}}
#' \item{\code{p.recomb}}{\code{double(1)}}
#' \item{\code{p.mut}}{\code{double(1)}}
#' \item{\code{survival.strategy}}{\code{character(1)}}
#' \item{\code{n.elite}}{\code{integer(1)}}
#' \item{\code{initial.solutions}}{list of \code{integer()}}
#' \item{\code{parent.selector}}{object of class [ecr::selector]}
#' \item{\code{survival.selector}}{object of class [ecr::selector]}}
#'
#' For the meaning of the control parameter, see [ecr::ecr()].
#' Note that `mu` and `lambda` must be set by the user.
#' The `terminators` parameter is replaced by the `Terminator` subclasses.
#'
#' @export
FSelectEvolutionary = R6Class("FSelectEvolutionary",
  inherit = FSelect,
  public = list(
    #' @description
    #' Create new `FSelectEvolutionary ` object.
    #' @return A `FSelectEvolutionary ` object.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("mu"),
        ParamInt$new("lambda"),
        ParamDbl$new("p.recomb", default = 0.7),
        ParamDbl$new("p.mut", default = 0.3),
        ParamFct$new("survival.strategy", default = "plus", levels = c("plus", "comma")),
        ParamDbl$new("n.elite", default = 0),
        ParamUty$new("initial.solutions", default = NULL),
        ParamFct$new("parent.selector", default = "selTournament", levels = c("selTournament", "selRoulette", "selRanking", "selGreedy")),
        ParamFct$new("survival.selector", default = "selGreedy", levels = c("selTournament", "selRoulette", "selRanking", "selGreedy")))
      )
      ps$add_dep("n.elite", "survival.strategy", CondEqual$new("comma"))

      super$initialize(
        param_set = ps
      )
    }
  ),
  private = list(
    select_internal = function(instance) {
      pars = self$param_set$values

      assert_numeric(pars$mu)
      assert_numeric(pars$lambda)

      pars$parent.selector = switch(pars$parent.selector,
        selTournament = ecr::selTournament,
        selRoulette = ecr::selRoulette,
        selRanking = ecr::selRanking,
        selGreedy = ecr::selGreedy,
        selNondom = ecr::selNondom,
        selDomHV = ecr::selDomHV)

      pars$survival.selector = switch(pars$survival.selector,
        selTournament = ecr::selTournament,
        selRoulette = ecr::selRoulette,
        selRanking = ecr::selRanking,
        selGreedy = ecr::selGreedy,
        selNondom = ecr::selNondom,
        selDomHV = ecr::selDomHV)

      ctrl = ecr::initECRControl(instance$fselect_objective, n.objectives = 1)
      ctrl = ecr::registerECROperator(ctrl, "mutate", ecr::mutBitflip, p = 0.1)
      ctrl = ecr::registerECROperator(ctrl, "recombinde", ecr::recCrossover)
      ctrl = ecr::registerECROperator(ctrl, "selectForMating", pars$parent.selector)
      ctrl = ecr::registerECROperator(ctrl, "selectForSurvival", pars$survival.selector)

      population = ecr::initPopulation(mu = pars$mu,
        gen.fun = ecr::genBin,
        initial.solutions = pars$initial.solutions,
        n.dim = length(instance$task$feature_names))
      population = map_if(population,
                          function(x) sum(x) == 0,
                          function(x) {
                            x[sample(1:length(x), 1)] = 1
                            x}) # Tasks without features cannot be evaluated

      withr::with_package("ecr", {
        fitness = ecr::evaluateFitness(ctrl, population)
      })

      while (TRUE) {
        offspring = ecr::generateOffspring(ctrl, population, fitness, pars$lambda, p.recomb = pars$p.recomb, p.mut = pars$p.mut)
        offspring = map_if(offspring,
                           function(x) sum(x) == 0,
                           function(x) {
                             x[sample(1:length(x), 1)] = 1
                             x})

        withr::with_package("ecr", {
          fitness_o = ecr::evaluateFitness(ctrl, offspring)
        })
        if (pars$survival.strategy == "plus") {
          selection = ecr::replaceMuPlusLambda(ctrl, population, offspring, fitness, fitness_o)
        } else {
          selection = ecr::replaceMuCommaLambda(ctrl, population, offspring, fitness, fitness_o, n.elite = pars$n.elite)
        }
        population = selection$population
      }
    },

    set_defaults = function(instance) {
      if (is.null(self$param_set$values$p.recomb)) self$param_set$values$p.recomb = self$param_set$default[["p.recomb"]]
      if (is.null(self$param_set$values$p.mut)) self$param_set$values$p.mut = self$param_set$default[["p.mut"]]
      if (is.null(self$param_set$values$survival.strategy)) self$param_set$values$survival.strategy = self$param_set$default[["survival.strategy"]]
      if (is.null(self$param_set$values$n.elite) && self$param_set$values$survival.strategy == "comma") self$param_set$values$n.elite = self$param_set$default[["n.elite"]]
      if (is.null(self$param_set$values$initial.solutions)) self$param_set$values$initial.solutions = self$param_set$default[["initial.solutions"]]
      if (is.null(self$param_set$values$parent.selector)) self$param_set$values$parent.selector = self$param_set$default[["parent.selector"]]
      if (is.null(self$param_set$values$survival.selector)) self$param_set$values$survival.selector = self$param_set$default[["survival.selector"]]
    }
  )
)

mlr_fselectors$add("evolutionary", FSelectEvolutionary)
