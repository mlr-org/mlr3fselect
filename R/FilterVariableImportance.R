#' @title Variable Importance Filter
#'
#' @name mlr_filters_variable_importance
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description
#' Variable Importance filter.
#' Takes a [mlr3::Learner] which supports retrieving the variable importance (property "importance"),
#' fits the model and uses the importance values as filter scores.
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' filter = FilterVariableImportance$new(learner = learner)
#' filter$calculate(task)
#' head(as.data.table(filter), 3)
FilterVariableImportance = R6Class("FilterVariableImportance", inherit = Filter,
  public = list(
    learner = NULL,
    initialize = function(id = "FilterVariableImportance", learner) {
      self$learner = assert_learner(learner, properties = "importance")

      super$initialize(
        id = id,
        packages = learner$packages,
        feature_types = learner$feature_types,
        task_type = learner$task_type,
        param_set = learner$param_set$clone(deep = TRUE)
      )
    }
  ),

  private = list(
    .calculate = function(task) {
      learner = self$learner$clone(deep = TRUE)
      learner$param_set$values = self$param_set$values
      e = Experiment$new(task = task, learner = learner)$train()
      importance = e$learner$importance()

      fn = task$feature_names
      insert_named(set_names(numeric(length(fn)), fn), importance)
    }
  )
)
