#' @title Information Gain Filter
#'
#' @name mlr_filters_information_gain
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Information gain filter.
#' Calls [FSelectorRcpp::information_gain()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("sonar")
#' filter = FilterInformationGain$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterInformationGain = R6Class("FilterInformationGain", inherit = Filter,
  public = list(
    initialize = function(id = "information_gain") {
      super$initialize(
        id = id,
        packages = "FSelectorRcpp",
        feature_types = c("numeric", "factor", "ordered"),
        task_type = c("classif", "regr")
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      x = setDF(task$data(cols = task$feature_names))
      y = task$truth()

      filter_values = invoke(FSelectorRcpp::information_gain,
        x = x, y = y, type = "infogain", .args = self$param_set$values)
      set_names(filter_values$importance, filter_values$attributes)
    }
  )
)
