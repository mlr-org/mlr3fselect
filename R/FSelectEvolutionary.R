#' @title FSelectEvolutionary Class
#'
#' @description
#' Subclass for evolutionary feature selection. Calls [ecr::ecr()] from package
#' \CRANpkg{ecr}.
#'
#' @section Parameters:
#' \describe{
#' \item{`mu`}{`integer(1)`}
#' \item{`lambda`}{`integer(1)`}
#' \item{`p.recomb`}{`double(1)`}
#' \item{`p.mut`}{`double(1)`}
#' \item{`survival.strategy`}{`character(1)`}
#' \item{`n.elite`}{`integer(1)`}
#' \item{`initial.solutions`}{list of `integer()`}
#' \item{`parent.selector`}{`ecr` selector}
#' \item{`survival.selector`}{`ecr` selector}
#' }
#'
#' For the meaning of the control parameter, see [ecr::ecr()]. Note that `mu`
#' and `lambda` must be set by the user. The `terminators` parameter is replaced
#' by the [Terminator] subclasses.
#'
#' @export
#' @templateVar fs "evolutionary", mu = 10, lambda = 5
#' @template example
FSelectEvolutionary = R6Class("FSelectEvolutionary",
  inherit = FSelect,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("mu"),
        ParamInt$new("lambda"),
        ParamDbl$new("p", default = 0.1, lower = 0, upper = 1),
        ParamDbl$new("p.recomb", default = 0.7, lower = 0, upper = 1),
        ParamDbl$new("p.mut", default = 0.1, lower = 0, upper = 1),
        ParamFct$new("survival.strategy", default = "plus",
          levels = c("plus", "comma")),
        ParamDbl$new("n.elite", default = 0),
        ParamUty$new("initial.solutions", default = NULL),
        ParamFct$new("parent.selector", default = "selTournament",
          levels = c("selTournament", "selRoulette", "selGreedy")),
        ParamFct$new("survival.selector", default = "selGreedy",
          levels = c("selTournament", "selRoulette", "selGreedy")))
      )
      ps$add_dep("n.elite", "survival.strategy", CondEqual$new("comma"))

      super$initialize(
        param_set = ps,
        packages = "ecr"
      )
    }
  ),
  private = list(
    select_internal = function(instance) {

      pars = self$param_set$values
      pars_mutBitflip =
        pars[which(names(pars) %in% formalArgs(ecr::mutBitflip))]
      pars_initPopulation =
        pars[which(names(pars) %in% formalArgs(ecr::initPopulation))]
      pars_generateOffspring =
        pars[which(names(pars) %in% formalArgs(ecr::generateOffspring))]
      pars_replaceMuCommaLambda =
        pars[which(names(pars) %in% formalArgs(ecr::replaceMuCommaLambda))]
      if (is.null(pars$parent.selector)) pars$parent.selector = "selTournament"
      if (is.null(pars$survival.selector)) pars$survival.selector = "selGreedy"
      if (is.null(pars$survival.strategy)) pars$survival.strategy = "plus"

      pars$parent.selector = switch(pars$parent.selector,
        selTournament = ecr::selTournament,
        selRoulette = ecr::selRoulette,
        selGreedy = ecr::selGreedy,
        selNondom = ecr::selNondom,
        selDomHV = ecr::selDomHV)

      pars$survival.selector = switch(pars$survival.selector,
        selTournament = ecr::selTournament,
        selRoulette = ecr::selRoulette,
        selGreedy = ecr::selGreedy,
        selNondom = ecr::selNondom,
        selDomHV = ecr::selDomHV)

      ctrl = ecr::initECRControl(objective_fun, n.objectives = 1)
      ctrl = invoke(ecr::registerECROperator, ctrl, "mutate",
        ecr::mutBitflip, .args = pars_mutBitflip)
      ctrl = ecr::registerECROperator(ctrl, "recombinde", ecr::recCrossover)
      ctrl = ecr::registerECROperator(ctrl, "selectForMating",
        pars$parent.selector)
      ctrl = ecr::registerECROperator(ctrl, "selectForSurvival",
        pars$survival.selector)

      population = invoke(ecr::initPopulation,
        gen.fun = ecr::genBin,
        n.dim = length(instance$task$feature_names),
        .args = pars_initPopulation)
      population = map_if(population,
        function(x) sum(x) == 0,
        function(x) {
          x[sample(1:length(x), 1)] = 1
          x
        }) # Tasks without features cannot be evaluated

      withr::with_package("ecr", {
        fitness = ecr::evaluateFitness(ctrl, population, instance)
      })

      while (TRUE) {
        offspring = invoke(ecr::generateOffspring, ctrl, population, fitness,
          .args = pars_generateOffspring)

        offspring = map_if(offspring,
          function(x) sum(x) == 0,
          function(x) {
            x[sample(1:length(x), 1)] = 1
            x
          })

        withr::with_package("ecr", {
          fitness_o = ecr::evaluateFitness(ctrl, offspring, instance)
        })
        if (pars$survival.strategy == "plus") {
          selection = ecr::replaceMuPlusLambda(ctrl, population, offspring,
            fitness, fitness_o)
        } else {
          selection = invoke(ecr::replaceMuCommaLambda,
            ctrl,
            population,
            offspring,
            fitness,
            fitness_o,
            .args = pars_replaceMuCommaLambda)
        }
        population = selection$population
      }
    }
  )
)

objective_fun = function(x, instance) {
  x = as.list(as.logical(x))
  names(x) = instance$task$feature_names
  x = as.data.table(x)

  res = instance$evaluator$eval_batch(x)
  as.numeric(res[,instance$measures[[1]]$id, with=FALSE])
}

mlr_fselectors$add("evolutionary", FSelectEvolutionary)
