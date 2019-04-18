#' @title Variance Filter
#'
#' @name mlr_filters_variance
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Variance filter.
#' Calls [stats::var()]. Argument `na.rm` defaults to `TRUE` here.
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("mtcars")
#' filter = FilterVariance$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterVariance = R6Class("FilterVariance", inherit = Filter,
  public = list(
    initialize = function(id = "FilterVariance", settings = list(na.rm = TRUE)) {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = "numeric",
        task_type = c("classif", "regr"),
        param_set = ParamSet$new(list(ParamLgl$new("na.rm", default = TRUE))),
        param_vals = list(na.rm = TRUE)
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      map_dbl(task$data(cols = task$feature_names), function(x) {
        invoke(var, x, .args = self$param_set$values)
      })
    }
  )
)
