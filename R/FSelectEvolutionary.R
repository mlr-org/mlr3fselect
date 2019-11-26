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
#' Note that `mu` is set to 25 by default, `lambda` to 30, `parent.selector` to [ecr::selTournament] and `survival.selector` to [ecr::selGreedy].
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
        ParamInt$new("mu", default = 25),
        ParamInt$new("lambda", default = 30),
        ParamDbl$new("p.recomb", default = 0.7),
        ParamDbl$new("p.mut", default = 0.3),
        ParamFct$new("survival.strategy", default = "plus", levels = c("plus", "comma")),
        ParamDbl$new("n.elite", default = 0),
        ParamUty$new("initial.solutions", default = NULL),
        ParamFct$new("parent.selector", default = "selTournament", levels = c("selTournament", "selRoulette", "selRanking", "selGreedy", "selNondom", "selDomHV")),
        ParamFct$new("survival.selector", default = "selGreedy", levels = c("selTournament", "selRoulette", "selRanking", "selGreedy", "selNondom", "selDomHV")))
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

      withr::with_package("ecr", {
        invoke(ecr::ecr, instance$fselect_objective,
          minimize = TRUE,
          n.objective = 1L,
          n.bits = length(instance$task$feature_names),
          representation = "binary",
          mutator = ecr::mutBitflip,
          recombinator = ecr::recCrossover,
          terminators = list(ecr::stopOnMaxTime(max.time = NULL)),
          .args = pars)
      })
    },

    set_defaults = function(instance) {
      if (is.null(self$param_set$values$mu)) self$param_set$values$mu = self$param_set$default[["mu"]]
      if (is.null(self$param_set$values$lambda)) self$param_set$values$lambda = self$param_set$default[["lambda"]]
      if (is.null(self$param_set$values$parent.selector)) self$param_set$values$parent.selector = self$param_set$default[["parent.selector"]]
      if (is.null(self$param_set$values$survival.selector)) self$param_set$values$survival.selector = self$param_set$default[["survival.selector"]]
    }
  )
)

mlr_fselectors$add("evolutionary", FSelectEvolutionary)
