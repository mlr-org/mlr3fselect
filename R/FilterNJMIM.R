#' @title Minimal Normalised Joint Mutual Information Maximisation Filter
#'
#' @aliases mlr_filters_njmim
#' @format [R6::R6Class] inheriting from [FilterResult].
#' @include Filter.R
#'
#' @description
#' Minimal normalised joint mutual information maximisation filter.
#' Calls [praznik::NJMIM()].
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' filter = FilterNJMIM$new()
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterNJMIM = R6Class("FilterNJMIM", inherit = FilterResult,
  public = list(
    initialize = function(id = "njmim") {
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
      praznik::NJMIM(X = X, Y = Y, k = ncol(X))$score
    }
  )
)

register_filter("njmim", FilterNJMIM)
