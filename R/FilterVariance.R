#' @title Variance
#'
#' @description
#' FilterVariance
#'
#' @section Usage:
#' ```
#' filter = FilterVariance$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterVariance].
#'
#' @name FilterVariance
#' @family Filter
#' @examples
#' task = mlr_tasks$get("trees")
#' filter = FilterVariance$new()
#' filter$calculate(task)
NULL

#' @export
#' @include Filter.R
FilterVariance = R6Class("FilterVariance", inherit = Filter,
  public = list(
    initialize = function(id = "FilterVariance", settings = list(na.rm = TRUE)) {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = "numeric",
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

      filter_values = map_dbl(task$feature_names, function(.x) {
        t = invoke(var, task$data(col = .x), .args = settings)
        #t$statistic
      })

      self$filter_values = sort(filter_values, decreasing = TRUE, na.last = TRUE)
    }
  )
)
