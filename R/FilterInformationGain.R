#' @title Gain Ratio
#'
#' @description
#' FilterGain Ratio
#'
#' @section Usage:
#' ```
#' filter = FilterInformationGain$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterInformationGain].
#'
#' @name FilterInformationGain
#' @family Filter
#' @examples
#' task = mlr3::mlr_tasks$get("sonar")
#' filter = FilterInformationGain$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterInformationGain = R6Class("FilterInformationGain", inherit = Filter,
  public = list(
    initialize = function(id = "FilterInformationGain", settings = list()) {
      super$initialize(
        id = id,
        packages = "FSelectorRcpp",
        feature_types = c("numeric", "factor", "ordered"),
        task_type = c("classif", "regr"),
        settings = settings)
    }
  ),

  private = list(
    .calculate = function(task, settings) {
      x = setDF(task$data(cols = task$feature_names))
      y = task$data(cols = task$target_names)[[task$target_names]]

      filter_values = invoke(FSelectorRcpp::information_gain,
        x = x, y = y, type = "infogain", .args = settings)
      set_names(filter_values$importance, filter_values$attributes)
    }
  )
)

#' @include mlr_filters.R
mlr_filters$add("FilterInformationGain", FilterInformationGain)
