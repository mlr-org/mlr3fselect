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
#' task = mlr_tasks$get("trees")
#' filter = FilterRankCorrelation$new()
#' filter$calculate(task)
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
    },
    calculate = function(task, settings = self$settings) {

      # check for supported features
      assert_feature_types(task, self)
      # check for supported task
      assert_filter(filter, task)

      # check for Namespace
      require_namespaces(self$packages)

      # assign task to class
      self$task = task

      browser()
      filter_values = abs(invoke(
        stats::cor,
        x = as.matrix(task$data(cols = task$feature_names)),
        y = as.matrix(task$data(cols = task$target_names)),
        use = "pairwise.complete.obs",
        method = "spearman",
        .args = settings)[, 1L])
      self$filter_values = sort(filter_values,
        decreasing = TRUE, na.last = TRUE)
    }
  )
)
