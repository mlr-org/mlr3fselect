#' @title Gain Ratio Filter
#'
#' @name mlr_filters_gain_ratio
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Gain Ratio filter.
#' Calls [FSelectorRcpp::information_gain()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("sonar")
#' filter = FilterGainRatio$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterGainRatio = R6Class("FilterGainRatio", inherit = Filter,
  public = list(
    initialize = function(id = "gain_ratio") {
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
        x = x, y = y, type = "gainratio")
      filter_values = set_names(filter_values$importance, filter_values$attributes)
      replace(filter_values, is.nan(filter_values), 0) # FIXME: this is a technical fix, need to report
    }
  )
)
