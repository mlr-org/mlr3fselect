#' @title Kruskal Test
#'
#' @description
#' FilterKruskalTest
#'
#' @section Usage:
#' ```
#' filter = FilterKruskalTest$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterKruskalTest].
#'
#' @name FilterKruskalTest
#' @family Filter
#' @examples
#' task = mlr_tasks$get("iris")
#' filter = FilterKruskalTest$new()
#' filter$calculate(task)
#' filter$filter(abs = 2)
NULL

#' @export
#' @include Filter.R
FilterKruskalTest = R6Class("FilterKruskalTest",
  inherit = Filter,
  public = list(
    initialize = function(id, packages,
      feature_types,
      task_type,
      settings = list()) {
      super$initialize(
        id = "FilterKruskalTest",
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

      filter_values = map_dbl(task$feature_names, function(.x) {
        f = as.formula(paste0(.x, "~", task$target_names))
        t = invoke(kruskal.test,
          f, data = task$data(), .args = settings)
        t$statistic
      })
      names(filter_values) = task$feature_names
      filter_values = replace(filter_values, is.nan(filter_values), 0) # FIXME: this is a technical fix, need to report
      self$filter_values = sort(filter_values, decreasing = TRUE,
        na.last = TRUE)
    }
  )
)

