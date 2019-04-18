#' @title Linear Correlation Filter
#'
#' @name mlr_filters_linear_correlation
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Linear correlation filter.
#' Calls [stats::cor()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("mtcars")
#' filter = FilterLinearCorrelation$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterLinearCorrelation = R6Class("FilterLinearCorrelation", inherit = Filter,
  public = list(
    initialize = function(id = "linear_correlation") {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = "numeric",
        task_type = "regr"
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      abs(invoke(
        stats::cor,
        x = as.matrix(task$data(cols = task$feature_names)),
        y = as.matrix(task$data(cols = task$target_names)),
        use = "pairwise.complete.obs",
        method = "pearson")[, 1L])
    }
  )
)
