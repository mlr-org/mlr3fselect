#' @title Joint Mutual Information Filter
#'
#' @aliases mlr_filters_jmi
#' @format [R6::R6Class] inheriting from [FilterResult].
#' @include Filter.R
#'
#' @description
#' Joint mutual information filter.
#' Calls [praznik::JMI()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterJMI$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterJMI = R6Class("FilterJMI", inherit = FilterResult,
  public = list(
    initialize = function(id = "jmi") {
      super$initialize(
        id = id,
        packages = "praznik",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        task_type = "classif"
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      X = task$data(cols = task$feature_names)
      Y = task$truth()
      praznik::JMI(X = X, Y = Y, k = ncol(X))$score
    }
  )
)

register_filter("jmi", FilterJMI)
