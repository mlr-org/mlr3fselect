#' @title Conditional Mutual Information Based Feature Selection Filter
#'
#' @aliases mlr_filters_carscore
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Filter `carscore` determines the Correlation-Adjusted (marginal) coRelation
#' scores (short CAR scores). The CAR scores for a set of features are defined as the
#' correlations between the target and the decorrelated features.
#'
#' @family Filter
#' @export
FilterCarScore = R6Class("FilterCarScore", inherit = Filter,
  public = list(
    initialize = function(id = "carscore") {
      super$initialize(
        id = id,
        packages = "care",
        feature_types = c("numeric"),
        task_type = "regr"
      )
    }
  ),

  private = list(
    .calculate = function(task) {

      # FIXME task splitting should really be easier
      data = as.data.table(task)
      target = task$target_names
      target = data[, ..target]

      features = task$feature_names
      features = as.data.frame(data[, ..features])

      y = care::carscore(Xtrain = features, Ytrain = target, verbose = FALSE)^2
      setNames(as.double(y), names(y))
    }
  )
)

register_filter("carscore", FilterCarScore)
