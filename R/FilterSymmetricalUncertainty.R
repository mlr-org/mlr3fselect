#' @title Symmetrical Uncertainty
#'
#' @description
#' FilterSymmetricalUncertainty
#'
#' @section Usage:
#' ```
#' filter = FilterSymmetricalUncertainty$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterSymmetricalUncertainty].
#'
#' @name FilterSymmetricalUncertainty
#' @family Filter
#' @examples
#' task = mlr_tasks$get("sonar")
#' filter = FilterSymmetricalUncertainty$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterSymmetricalUncertainty = R6Class("FilterSymmetricalUncertainty", inherit = Filter,
  public = list(
    initialize = function(id = "FilterSymmetricalUncertainty", settings = list()) {
      super$initialize(
        id = id,
        packages = "FSelectorRcpp",
        feature_types = c("numeric", "integer", "factor", "ordered"),
        task_type = c("classif", "regr"),
        settings = settings)
    }
  ),
  
  private = list(
    .calculate = function(task, settings = self$settings) {
      x = as.data.frame(task$data(cols = task$feature_names))
      y = task$data(cols = task$target_names)[[task$target_names]]

      fv = invoke(FSelectorRcpp::information_gain,
        x = x, y = y, type = "symuncert", .args = settings)
      set_names(fv$importance, fv$attributes)
    }
  )
)

