#' @title AUC Filter
#'
#' @name mlr_filters_auc
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Area under the (ROC) Curve filter.
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("sonar")
#' filter = FilterAUC$new()
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterAUC = R6Class("FilterAUC", inherit = Filter,
  public = list(
    initialize = function(id = "auc") {
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
      score = map_dbl(task$data(col = task$feature_names), function(x, y) {
        measureAUC(x, y, task$negative, task$positive)
      }, y = task$data(col = task$target_names)[[task$target_names]])
      abs(0.5 - score)
    }
  )
)

measureAUC = function(probabilities, truth, negative, positive) {
  if (is.factor(truth)) {
    i = as.integer(truth) == which(levels(truth) == positive)
  } else {
    i = truth == positive
  }

  if (uniqueN(i) < 2L)
    stopf("truth vector must have at least two classes")

  r = frankv(probabilities)
  n.pos = as.numeric(sum(i))
  n.neg = length(i) - n.pos
  (sum(r[i]) - n.pos * (n.pos + 1) / 2) / (n.pos * n.neg)
}
