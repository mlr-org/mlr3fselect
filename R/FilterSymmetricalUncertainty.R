#' @title Symmetrical Uncertainty Filter
#'
#' @name mlr_filters_symmetrical_uncertainty
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Symmetrical uncertainty filter.
#' Calls [FSelectorRcpp::information_gain()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("sonar")
#' filter = FilterSymmetricalUncertainty$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterSymmetricalUncertainty = R6Class("FilterSymmetricalUncertainty", inherit = Filter,
  public = list(
    initialize = function(id = "symmetrical_uncertainty") {
      super$initialize(
        id = id,
        packages = "FSelectorRcpp",
        feature_types = c("integer", "numeric", "integer", "factor", "ordered"),
        task_type = c("classif", "regr")
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      x = as.data.frame(task$data(cols = task$feature_names))
      y = task$truth()

      fv = FSelectorRcpp::information_gain(x = x, y = y, type = "symuncert")
      set_names(fv$importance, fv$attributes)
    }
  )
)
