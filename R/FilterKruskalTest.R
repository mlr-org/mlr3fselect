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
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterKruskalTest$new()
#' filter$calculate(task)
#' filter$filter_abs(task, 2)
#' head(as.data.table(filter), 3)
NULL

#' @export
#' @include Filter.R
FilterKruskalTest = R6Class("FilterKruskalTest", inherit = Filter,
  public = list(
    initialize = function(id = "FilterKruskalTest", settings = list()) {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = "numeric",
        task_type = "classif",
        settings = settings)
    }
  ),

  private = list(
    .calculate = function(task, settings) {
      data = task$data(cols = task$feature_names)
      g = task$truth()
      filter_values = map_dbl(data, function(x) {
        invoke(kruskal.test, x = x, g = task$truth(), .args = settings)$statistic
      })
      replace(filter_values, is.nan(filter_values), 0) # FIXME: this is a technical fix, need to report
    }
  )
)

#' @include mlr_filters.R
mlr_filters$add("FilterKruskalTest", FilterKruskalTest)
