#' @title AUC
#'
#' @description
#' FilterAUC
#'
#' @section Usage:
#' ```
#' filter = FilterAUC$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterAUC].
#'
#' @name FilterAUC
#' @family Filter
#' @examples
#' task = mlr_tasks$get("spam")
#' filter = FilterAUC$new()
#' filter$calculate(task)
NULL

#' @export
#' @include Filter.R
FilterAUC = R6Class("FilterAUC", inherit = Filter,
  public = list(
    initialize = function(id = "FilterAUC", settings = list()) {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = "numeric",
        task_type = "classif",
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

      score = map_dbl(task$data(col = task$feature_names), function(x, y) {
        measureAUC(x, y, task$negative, task$positive)
      }, y = task$data(col = task$target_names)[[task$target_names]])
      filter_values = abs(0.5 - score)

      self$filter_values = sort(filter_values, decreasing = TRUE, na.last = TRUE)
    }
  )
)
