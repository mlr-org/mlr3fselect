#' @title FSelectorFromOptimizer
#'
#' @description
#' Internally used to transform [bbotk::Optimizer] to [FSelector].
#'
#' @keywords internal
#' @export
FSelectorFromOptimizer = R6Class("FSelectorFromOptimizer",
  inherit = FSelector,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param optimizer [bbotk::Optimizer]\cr
    #' Optimizer that is called.
    initialize = function(optimizer) {
      private$.optimizer = assert_optimizer(optimizer)
      super$initialize(param_set = optimizer$param_set,
        properties = optimizer$properties)
    },

    #' @description
    #' Performs the tuning on a [FSelectInstanceSingleCrit] /
    #' [FSelectInstanceMultiCrit] until termination.
    #'
    #' @param inst ([FSelectInstanceSingleCrit] | [FSelectInstanceMultiCrit]).
    #'
    #' @return [data.table::data.table].
    optimize = function(inst) {
      # We check for both classes since there is no FSelectInstance super
      # class anymore and OptimInstance would not ensure that we are in the
      # scope of mlr3tuning
      assert_multi_class(inst, c("FSelectInstanceSingleCrit", "FSelectInstanceMultiCrit"))
      private$.optimizer$optimize(inst)
    }
  ),

  private = list(
    .optimizer = NULL
  )
)
