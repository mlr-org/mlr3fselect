#' @title Linear Correlation Filter
#'
#' @aliases mlr_filters_linear_correlation
#' @format [R6::R6Class] inheriting from [FilterResult].
#' @include FilterResult.R
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
#' as.data.table(filter)[1:3]
FilterLinearCorrelation = R6Class("FilterLinearCorrelation", inherit = FilterResult,
  public = list(
    initialize = function(id = "linear_correlation") {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = c("integer", "numeric"),
        task_type = "regr"
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      abs(stats::cor(
        x = as.matrix(task$data(cols = task$feature_names)),
        y = as.matrix(task$data(cols = task$target_names)),
        use = "pairwise.complete.obs",
        method = "pearson")[, 1L])
    }
  )
)

register_filter("linear_correlation", FilterLinearCorrelation)
