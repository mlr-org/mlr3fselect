#' @title Joint mutual information
#'
#' @description
#' 	Joint mutual information filter
#'
#' @section Usage:
#' ```
#' filter = FilterJMI$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterJMI].
#'
#' @name FilterJMI
#' @family Filter
#' @examples
#' task = mlr_tasks$get("iris")
#' filter = FilterJMI$new()
#' filter$calculate(task)
NULL

#' @export
#' @include Filter.R
FilterJMI = R6Class("FilterJMI",
  inherit = Filter,
  public = list(
    initialize = function(id, packages,
      feature_types,
      task_type,
      settings = list()) {
      super$initialize(
        id = "FilterJMI",
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
        praznik::JMI, X = X, Y = Y, k = ncol(X), .args = settings)$score

      self$filter_values = sort(filter_values, decreasing = TRUE,
        na.last = TRUE)
    }
  )
)

