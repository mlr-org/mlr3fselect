#' @title Linear Correlation
#'
#' @description
#' FilterLinearCorrelation
#'
#' @section Usage:
#' ```
#' filter = FilterLinearCorrelation$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterLinearCorrelation].
#'
#' @name FilterLinearCorrelation
#' @family Filter
#' @examples
#' task = mlr_tasks$get("mtcars")
#' filter = FilterLinearCorrelation$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterLinearCorrelation = R6Class("FilterLinearCorrelation", inherit = Filter,
  public = list(
    initialize = function(id = "FilterLinearCorrelation", settings = list()) {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = "numeric",
        task_type = "regr",
        settings = settings)
    }
  ),
  
  private = list(
    .calculate = function(task, settings = self$settings) {
      abs(invoke(
        stats::cor,
        x = as.matrix(task$data(cols = task$feature_names)),
        y = as.matrix(task$data(cols = task$target_names)),
        use = "pairwise.complete.obs",
        method = "pearson",
        .args = settings)[, 1L])
    }
  )
)
