#' @title Minimal conditional mutual information
#'
#' @description
#' 	Minimal conditional mutual information maximisation filter
#'
#' @section Usage:
#' ```
#' filter = FilterCMIM$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterCMIM].
#'
#' @name FilterCMIM
#' @family Filter
#' @examples
#' task = mlr_tasks$get("iris")
#' filter = FilterCMIM$new()
#' filter$calculate(task)
NULL

#' @export
#' @include Filter.R
FilterCMIM = R6Class("FilterCMIM", inherit = Filter,
  public = list(
    initialize = function(id = "FilterCMIM", settings = list()) {
      super$initialize(
        id = id,
        packages = "praznik",
        feature_types = c("numeric", "factor", "ordered"),
        task_type = c("classif", "regr"),
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
        praznik::CMIM,
        X = X, Y = Y, k = ncol(X), .args = settings)$score

      self$filter_values = sort(filter_values, decreasing = TRUE,
        na.last = TRUE)
    }
  )
)

