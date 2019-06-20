#' @title Kruskal-Wallis Test Filter
#'
#' @aliases mlr_filters_kruskal_test
#' @format [R6::R6Class] inheriting from [FilterResult].
#' @include FilterResult.R
#'
#' @description
#' Kruskal-Wallis rank sum test filter.
#' Calls [stats::kruskal.test()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterKruskalTest$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterKruskalTest = R6Class("FilterKruskalTest", inherit = FilterResult,
  public = list(
    initialize = function(id = "kruskal_test") {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = c("integer", "numeric"),
        task_type = "classif"
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      data = task$data(cols = task$feature_names)
      g = task$truth()
      scores = map_dbl(data, function(x) {
        kruskal.test(x = x, g = g)$statistic
      })
      replace(scores, is.nan(scores), 0) # FIXME: this is a technical fix, need to report
    }
  )
)

register_filter("kruskal_test", FilterKruskalTest)
