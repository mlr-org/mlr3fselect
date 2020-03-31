#' FSelect
#'
#' @include mlr_fselectors.R
#'
#' @description
#' Abstract `FSelect` class that implements the base functionality each `FSelect*` class must provide.
#' A `FSelect` object describes the feature selection strategy,
#' i.e. how to optimize the black-box function
#' and its feasible set defined by the [FSelectInstance] object.
#'
#' A list of measures can be passed to the instance, and they will always be all evaluated.
#' However, single-criteria algorithms optimize only the first measure
#'
#' A `FSelect` object must write its result to the `$assign_result()` method of the [FSelectInstance] at the end in
#' order to store the best selected feature subset and its estimated performance vector.
#'
#' @export
#' @examples
#' library(mlr3)
#'
#' terminator = term("evals", n_evals = 15)
#' instance = FSelectInstance$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measures = msr("classif.ce"),
#'   terminator = terminator
#' )
#'
#' fs = fs("random") # swap this line to use a different feature selection algorithm
#' fs$select(instance) # modifies the instance by reference
#' instance$result # returns best configuration and best performance
#' instance$archive() # allows access of data.table / benchmark result of full path of all evaluation
FSelect = R6Class("FSelect",
  public = list(
    #' @field param_set [paradox::ParamSet]
    #' @field packages `character()`
    param_set = NULL,
    packages = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param param_set [paradox::ParamSet]
    #' @param packages `character()`
    #' Set of control parameter for the feature selection.
    initialize = function(param_set, packages = character()) {
      self$param_set = assert_param_set(param_set)
      self$packages = assert_set(packages)
    },

    #' @description
    #' Format method.
    #' @return `character()`
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Print method.
    #' @return `character()`
    print = function() {
      catf(format(self))
      catf(str_indent("* Parameters:", as_short_string(self$param_set$values)))
      catf(str_indent("* Packages:", self$packages))
    },

    #' @description
    #' Performs the feature selection on a [FSelectInstance] until termination.
    #' @param instance [FSelectInstance]
    select = function(instance) {
      assert_r6(instance, "FSelectInstance")
      require_namespaces(self$packages)

      lg$info("Starting feature selection")

      tryCatch({
        while (TRUE) {
          private$select_internal(instance)
        }
      }, terminated_error = function(cond) {
      })

      private$assign_result(instance)
      invisible(NULL)
    }
  ),

  private = list(
    select_internal = function() {
      # Implemented by subclass
      stop("Abstract")
    },

    assign_result = function(instance) {
      fselect_assign_result_default(instance)
    }
  )
)

fselect_assign_result_default = function(instance) {
  assert_r6(instance, "FSelectInstance")

  res = instance$evaluator$archive$get_best()
  feat = instance$task$feature_names[as.matrix(res[,instance$task$feature_names,with=FALSE])]
  perf = as.matrix((res[,instance$evaluator$objective$codomain$ids(),with=FALSE]))[1,]

  instance$assign_result(feat, perf)
  invisible(NULL)
}
