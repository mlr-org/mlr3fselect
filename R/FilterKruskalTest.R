#' @title Kruskal-Wallis Test Filter
#'
#' @aliases mlr_filters_kruskal_test
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description Kruskal-Wallis rank sum test filter. Calls
#'   [stats::kruskal.test()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterKruskalTest$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterKruskalTest = R6Class("FilterKruskalTest", inherit = Filter,
  public = list(
    initialize = function(id = "kruskal_test", param_vals = list()) {
      super$initialize(
        id = id,
        packages = "stats",
        feature_types = c("integer", "numeric"),
        task_type = "classif",
        param_set = ParamSet$new(list(
          ParamFct$new("na.action", default = "na.omit",
            levels = c("na.omit", "na.fail", "na.exclude", "na.pass"), tags = "filter"),
          ParamInt$new("abs", lower = 1, tags = "generic"),
          ParamDbl$new("perc", lower = 0, upper = 1, tags = "generic"),
          ParamDbl$new("thresh", tags = "generic")
        )),
        param_vals = param_vals
      )
    }
  ),

  private = list(
    .calculate = function(task, n = NULL) {

      # setting params
      na.action = self$param_set$values$na.action

      if (is.null(na.action)) {
        na.action = self$param_set$default$na.action
      }

      data = task$data(cols = task$feature_names)
      g = task$truth()

      scores = map_dbl(data, function(x) {
        kruskal.test(x = x, g = g, na.action = na.action)$statistic
      })
      replace(scores, is.nan(scores), 0) # FIXME: this is a technical fix, need to report
    }
  )
)

register_filter("kruskal_test", FilterKruskalTest)
