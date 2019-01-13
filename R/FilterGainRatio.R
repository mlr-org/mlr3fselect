#' @title Gain Ratio
#'
#' @description
#' FilterGainRatio
#'
#' @section Usage:
#' ```
#' filter = FilterGainRatio$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterGainRatio].
#'
#' @name FilterGainRatio
#' @family Filter
#' @examples
#' task = mlr3::mlr_tasks$get("sonar")
#' filter = FilterGainRatio$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterGainRatio = R6Class("FilterGainRatio", inherit = Filter,
  public = list(
    initialize = function(id = "FilterGainRatio", settings = list()) {
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
        x = x, y = y, type = "gainratio")
      filter_values = setNames(filter_values$importance,
        filter_values$attributes)
      replace(filter_values, is.nan(filter_values), 0) # FIXME: this is a technical fix, need to report
    }
  )
)

