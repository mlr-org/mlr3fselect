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
    initialize = function(id = "FilterVariance", param_vals = list(na.rm = TRUE)) {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = c("integer", "numeric"),
        task_type = c("classif", "regr"),
        param_set = ParamSet$new(list(ParamLgl$new("na.rm", default = TRUE, tags = "required"))),
        param_vals = param_vals
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      na.rm = self$param_set$values$na.rm
      map_dbl(task$data(cols = task$feature_names), function(x) {
        var(x, na.rm = na.rm)
      })
    }
  )
)
