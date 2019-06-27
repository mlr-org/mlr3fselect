#' @title Information Gain Filter
#'
#' @aliases mlr_filters_information_gain
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Information gain filter.
#' Calls [FSelectorRcpp::information_gain()].
#' The target variable of regression tasks is automatically binned (argument `equal` in [FSelectorRcpp::information_gain()]).
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("sonar")
#' filter = FilterInformationGain$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterInformationGain = R6Class("FilterInformationGain", inherit = Filter,
  public = list(
    initialize = function(id = "information_gain") {
      super$initialize(
        id = id,
        packages = "FSelectorRcpp",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        task_type = c("classif", "regr")
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      x = setDF(task$data(cols = task$feature_names))
      y = task$truth()
      scores = FSelectorRcpp::information_gain(x = x, y = y, type = "infogain", equal = task$task_type == "regr")
      set_names(scores$importance, scores$attributes)
    }
  )
)

register_filter("information_gain", FilterInformationGain)
