#' @title Conditional Mutual Information Based Feature Selection Filter
#'
#' @aliases mlr_filters_carscore
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description Filter `carscore` determines the Correlation-Adjusted (marginal)
#' coRelation scores (short CAR scores). The CAR scores for a set of features
#' are defined as the correlations between the target and the decorrelated
#' features.
#'
#' @family Filter
#' @examples
#' task = mlr3::mlr_tasks$get("mtcars")
#' filter = FilterCarScore$new()
#' filter$calculate(task)
#' @export
FilterCarScore = R6Class("FilterCarScore", inherit = Filter,
  public = list(
    initialize = function(id = "carscore", param_vals = list()) {
      super$initialize(
        id = id,
        packages = "care",
        feature_types = c("numeric"),
        task_type = "regr",
        param_set = ParamSet$new(list(
          ParamDbl$new("lambda", lower = 0, upper = 1, default = NO_DEF),
          ParamLgl$new("diagonal", default = FALSE),
          ParamLgl$new("verbose", default = FALSE))),
        param_vals = param_vals
      )
    }
  ),

  private = list(
    .calculate = function(task, n = NULL) {

      # FIXME task splitting should really be easier
      data = as.data.table(task)
      target = task$target_names
      target = data[, ..target]

      features = task$feature_names
      features = as.data.frame(data[, ..features])

      # setting params
      lambda = self$param_set$values$lambda
      diagonal = self$param_set$values$diagonal
      verbose = self$param_set$values$verbose

      if (is.null(diagonal)) {
        diagonal = self$param_set$default$diagonal
      }
      if (is.null(verbose)) {
        verbose = self$param_set$default$verbose
      }

      # since lambda has no default, we need to split the execution on it
      if (is.null(lambda)) {
        y = care::carscore(Xtrain = features, Ytrain = target,
          diagonal = diagonal, verbose = verbose)^2
      } else {
        y = care::carscore(Xtrain = features, Ytrain = target, lambda = lambda,
          diagonal = diagonal, verbose = verbose)^2
      }

      setNames(as.double(y), names(y))
    }
  )
)

register_filter("carscore", FilterCarScore)
