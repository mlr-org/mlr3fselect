#' @title Minimal normalised joint mutual information maximisation
#'
#' @description
#' 	Minimal normalised joint mutual information maximisation filter
#'
#' @section Usage:
#' ```
#' filter = FilterNJMIM$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterNJMIM].
#'
#' @name FilterNJMIM
#' @family Filter
#' @examples
#' task = mlr_tasks$get("iris")
#' filter = FilterNJMIM$new()
#' filter$calculate(task)
NULL

#' @export
#' @include Filter.R
FilterNJMIM = R6Class("FilterNJMIM", inherit = Filter,
  public = list(
    initialize = function(id = "FilterNJMIM", settings = list()) {
      super$initialize(
        id = id,
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
        praznik::NJMIM, X = X, Y = Y, k = ncol(X), .args = settings)$score

      self$filter_values = sort(filter_values, decreasing = TRUE,
        na.last = TRUE)
    }
  )
)

