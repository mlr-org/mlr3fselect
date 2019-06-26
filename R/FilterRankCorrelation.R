#' @title Rank Correlation Filter
#'
#' @aliases mlr_filters_rank_correlation
#' @format [R6::R6Class] inheriting from [FilterResult].
#' @include FilterResult.R
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
#' as.data.table(filter)[1:3]
FilterRankCorrelation = R6Class("FilterRankCorrelation", inherit = FilterResult,
  public = list(
    initialize = function(id = "rank_correlation") {
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
      fn = task$feature_names
      m = abs(stats::cor(
        x = as.matrix(task$data(cols = task$feature_names)),
        y = as.matrix(task$data(cols = task$target_names)),
        use = "pairwise.complete.obs",
        method = "spearman")[, 1L])
      set_names(m, fn)
    }
  )
)

register_filter("rank_correlation", FilterRankCorrelation)
