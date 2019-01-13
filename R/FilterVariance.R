#' @title Variance
#'
#' @description
#' FilterVariance
#'
#' @section Usage:
#' ```
#' filter = FilterVariance$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterVariance].
#'
#' @name FilterVariance
#' @family Filter
#' @examples
#' task = mlr_tasks$get("mtcars")
#' filter = FilterVariance$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterVariance = R6Class("FilterVariance", inherit = Filter,
  public = list(
    initialize = function(id = "FilterVariance", settings = list(na.rm = TRUE)) {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = "numeric",
        task_type = c("classif", "regr"),
        settings = settings)
    }
  ),
  
  private = list(
    .calculate = function(task, settings = self$settings) {
      map_dbl(task$data(cols = task$feature_names), function(x) {
        t = invoke(var, x, .args = settings)
      })
    }
  )
)
