#' @title FSelect
#'
#' @description
#' Abstract `FSelect` class that implements the base functionality each
#' `FSelect` subclass must provide. A `FSelect` object describes the feature
#' selection strategy, i.e. how to optimize the black-box function and its
#' feasible set defined by the [ObjectiveFSelect] object.
#'
#' A list of measures can be passed to the instance, and they will always be all
#' evaluated. However, single-criteria algorithms optimize only the first
#' measure
#'
#' A `FSelect` object must provide a private method `$.assign_result(inst)` that writes the result to the [FSelectInstance] using the `$assign_result()` method of
#' the instance in order to store the best selected feature
#' subset and its estimated performance vector at the end of the optimization.
#'
#' @export
#' @templateVar id random
#' @template example
FSelect = R6Class("FSelect",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param param_set [paradox::ParamSet]
    #' @param param_classes `character()`
    #' @param properties `character()`
    #' @param packages `character()`
    initialize = function(param_set, properties, packages = character(0)) {
      super$initialize(param_set = param_set, param_classes = "ParamLgl",
        properties = properties)
    },

    #' @description
    #' Performs the feature selection on a [FSelectInstance] until termination.
    #' @param inst [FSelectInstance]
    optimize = function(inst) {
      assert_r6(inst, "OptimInstance")
      require_namespaces(self$packages)

      tryCatch({
        repeat {
          private$.optimize(inst)
        }
      }, terminated_error = function(cond) { })

      private$.assign_result(inst)
      invisible(NULL)
    }
  )
)
