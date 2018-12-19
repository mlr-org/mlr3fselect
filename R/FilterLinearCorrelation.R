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
#' task = mlr_tasks$get("bh")
#' filter = FilterLinearCorrelation$new()
#' filter$calculate(task)
NULL

#' @export
#' @include Filter.R
FilterLinearCorrelation = R6Class("FilterLinearCorrelation",
  inherit = Filter,
  public = list(
    initialize = function(id, packages,
      feature_types,
      task_type,
      settings = list()) {
      super$initialize(
        id = "FilterLinearCorrelation",
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

      filter_values = abs(invoke(
        stats::cor,
        x = as.matrix(task$data(cols = task$feature_names)),
        y = as.matrix(task$data(cols = task$target_names)),
        use = "pairwise.complete.obs",
        method = "pearson")[, 1L],
        .args = settings)
      self$filter_values = sort(filter_values,
        decreasing = TRUE, na.last = TRUE)
    }
  )
)
