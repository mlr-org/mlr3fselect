#' @title Double input symmetrical relevance
#'
#' @description
#' 	Double input symmetrical relevance filter
#'
#' @section Usage:
#' ```
#' filter = FilterDISR$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterDISR].
#'
#' @name FilterDISR
#' @family Filter
#' @examples
#' task = mlr_tasks$get("iris")
#' filter = FilterDISR$new()
#' filter$calculate(task)
NULL

#' @export
#' @include Filter.R
FilterDISR = R6Class("FilterDISR",
  inherit = Filter,
  public = list(
    initialize = function(id, packages,
      feature_types,
      task_type,
      settings = list()) {
      super$initialize(
        id = "FilterDISR",
        packages = "praznik",
        feature_types = c("numeric", "factor", "ordered"),
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

      X = task$data(cols = task$feature_names)
      Y = task$data(cols = task$target_names)[[task$target_names]]
      filter_values = invoke(
        praznik::DISR, X = X, Y = Y, k = ncol(X), .args = settings)$score

      self$filter_values = sort(filter_values, decreasing = TRUE,
        na.last = TRUE)
    }
  )
)

