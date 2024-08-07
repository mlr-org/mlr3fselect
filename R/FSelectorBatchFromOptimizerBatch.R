#' @title FSelectorBatchFromOptimizerBatch
#'
#' @description
#' Internally used to transform [bbotk::Optimizer] to [FSelector].
#'
#' @template param_man
#'
#' @keywords internal
#' @export
FSelectorBatchFromOptimizerBatch= R6Class("FSelectorBatchFromOptimizerBatch",
  inherit = FSelectorBatch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param optimizer [bbotk::Optimizer]\cr
    #' Optimizer that is called.
    initialize = function(optimizer, man = NA_character_) {
      private$.optimizer = assert_optimizer(optimizer)
      packages = union("mlr3fselect", optimizer$packages)
      assert_string(man, na.ok = TRUE)

      super$initialize(
        id = if ("id" %in% names(optimizer)) optimizer$id else "optimizer",
        param_set = optimizer$param_set,
        properties = optimizer$properties,
        packages = packages,
        label = optimizer$label,
        man = man
       )
    },

    #' @description
    #' Performs the feature selection on a [FSelectInstanceBatchSingleCrit] /
    #' [FSelectInstanceBatchMultiCrit] until termination.
    #'
    #' @param inst ([FSelectInstanceBatchSingleCrit] | [FSelectInstanceBatchMultiCrit]).
    #'
    #' @return [data.table::data.table].
    optimize = function(inst) {
      # We check for both classes since there is no FSelectInstance super
      # class anymore and OptimInstance would not ensure that we are in the
      # scope of mlr3fselect
      assert_fselect_instance_batch(inst)
      result = private$.optimizer$optimize(inst)
      inst$objective$.__enclos_env__$private$.xss = NULL
      inst$objective$.__enclos_env__$private$.design = NULL
      inst$objective$.__enclos_env__$private$.benchmark_result = NULL
      inst$objective$.__enclos_env__$private$.aggregated_performance = NULL
      return(result)
    }
  ),

  private = list(
    .optimizer = NULL
  )
)
