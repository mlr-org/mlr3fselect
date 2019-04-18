#' @title Rank Correlation Filter
#'
#' @name mlr_filters_rank_correlation
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Rank correlation filter.
#' Calls [stats::cor()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("mtcars")
#' filter = FilterRankCorrelation$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterRankCorrelation = R6Class("FilterRankCorrelation", inherit = Filter,
  public = list(
    initialize = function(id = "rank_correlation") {
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
        method = "spearman")[, 1L])
    }
  )
)
