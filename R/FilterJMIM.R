#' @title Minimal Joint Mutual Information Maximisation Filter
#'
#' @aliases mlr_filters_jmim
#' @format [R6::R6Class] inheriting from [FilterResult].
#' @include Filter.R
#'
#' @description
#' Minimal joint mutual information maximisation filter.
#' Calls [praznik::JMIM()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterJMIM$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterJMIM = R6Class("FilterJMIM", inherit = FilterResult,
  public = list(
    initialize = function(id = "jmim") {
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
      praznik::JMIM(X = X, Y = Y, k = ncol(X))$score
    }
  )
)
