#' @title Rank Correlation
#'
#' @description
#' FilterRankCorrelation
#'
#' @section Usage:
#' ```
#' filter = FilterRankCorrelation$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterRankCorrelation].
#'
#' @name FilterRankCorrelation
#' @family Filter
#' @examples
#' task = mlr_tasks$get("mtcars")
#' filter = FilterRankCorrelation$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterRankCorrelation = R6Class("FilterRankCorrelation", inherit = Filter,
  public = list(
    initialize = function(id = "FilterRankCorrelation", settings = list()) {
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
        method = "spearman",
        .args = settings)[, 1L])
    }
  )
)
