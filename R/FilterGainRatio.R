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
        feature_types = c("integer", "numeric", "factor", "ordered"),
        task_type = c("classif", "regr")
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      x = setDF(task$data(cols = task$feature_names))
      y = task$truth()

      scores = invoke(FSelectorRcpp::information_gain,
        x = x, y = y, type = "gainratio")
      scores = set_names(scores$importance, scores$attributes)
      replace(scores, is.nan(scores), 0) # FIXME: this is a technical fix, need to report
    }
  )
)
