#' @title Filter which uses learner integrated methods
#'
#' @aliases mlr_filters_variable_importance
#' @format [R6::R6Class] inheriting from [Filter].
#' @include Filter.R
#'
#' @description Variable Importance filter using learner embedded methods. Takes
#'   a [mlr3::Learner] which supports retrieving the variable importance
#'   (property "importance"), fits the model and uses the importance values as
#'   filter scores.
#'
#'
#' @family Filter
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' filter = FilterEmbedded$new(learner = learner)
#' filter$calculate(task)
#' as.data.table(filter)[1:3]
FilterEmbedded = R6Class("FilterEmbedded", inherit = Filter,
  public = list(
    learner = NULL,
    initialize = function(id = "embedded", learner = "classif.rpart", param_vals = list()) {
      self$learner = learner = assert_learner(learner, properties = "importance")

      super$initialize(
        id = id,
        packages = learner$packages,
        feature_types = learner$feature_types,
        task_type = learner$task_type,
        param_set = ParamSet$new(list(
          ParamInt$new("nfeat", lower = 1, tags = "generic"),
          ParamDbl$new("frac", lower = 0, upper = 1, tags = "generic"),
          ParamDbl$new("cutoff", tags = "generic")
        )),
        param_vals = param_vals
      )
    }
  ),

  private = list(
    .calculate = function(task, n = NULL) {
      learner = self$learner$clone(deep = TRUE)
      learner = learner$train(task = task)
      importance = learner$importance()

      fn = task$feature_names
      insert_named(set_names(numeric(length(fn)), fn), importance)
    }
  )
)

register_filter("embedded", FilterEmbedded)
