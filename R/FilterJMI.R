#' @title Joint Mutual Information Filter
#'
#' @name mlr_filters_jmi
#' @format [R6::R6Class] inheriting from [Filter].
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
#' head(as.data.table(filter), 3)
FilterJMI = R6Class("FilterJMI", inherit = Filter,
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
      invoke(praznik::JMI, X = X, Y = Y, k = ncol(X), .args = self$param_set$values)$score
    }
  )
)
