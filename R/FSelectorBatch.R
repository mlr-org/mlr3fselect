#' @title Class for Batch Feature Selection Algorithms
#'
#' @include mlr_fselectors.R
#'
#' @description
#' The [FSelectorBatch] implements the optimization algorithm.
#'
#' @details
#' [FSelectorBatch] is an abstract base class that implements the base functionality each fselector must provide.
#' A subclass is implemented in the following way:
#'  * Inherit from FSelectorBatch.
#'  * Specify the private abstract method `$.optimize()` and use it to call into your optimizer.
#'  * You need to call `instance$eval_batch()` to evaluate design points.
#'  * The batch evaluation is requested at the [FSelectInstanceBatchSingleCrit]/[FSelectInstanceBatchMultiCrit] object `instance`, so each batch is possibly executed in parallel via [mlr3::benchmark()], and all evaluations are stored inside of `instance$archive`.
#'  * Before the batch evaluation, the [bbotk::Terminator] is checked, and if it is positive, an exception of class `"terminated_error"` is generated.
#'    In the latter case the current batch of evaluations is still stored in `instance`, but the numeric scores are not sent back to the handling optimizer as it has lost execution control.
#'  * After such an exception was caught we select the best set from `instance$archive` and return it.
#'  * Note that therefore more points than specified by the [bbotk::Terminator] may be evaluated, as the Terminator is only checked before a batch evaluation, and not in-between evaluation in a batch.
#'    How many more depends on the setting of the batch size.
#'  * Overwrite the private super-method `.assign_result()` if you want to decide how to estimate the final set in the instance and its estimated performance.
#'    The default behavior is: We pick the best resample experiment, regarding the given measure, then assign its set and aggregated performance to the instance.
#'
#' @section Private Methods:
#' * `.optimize(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify feature selection of your subclass.
#'   See technical details sections.
#' * `.assign_result(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify how the final feature subset is selected.
#'   See technical details sections.
#'
#' @inheritSection FSelector Resources
#'
#' @template param_id
#' @template param_param_set
#' @template param_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @export
FSelectorBatch = R6Class("FSelectorBatch",
  inherit = FSelector,
  public = list(

    #' @description
    #'   Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "fselector_batch",
      param_set,
      properties,
      packages = character(),
      label = NA_character_,
      man = NA_character_
      ) {
      super$initialize(
        id = id,
        param_set = param_set,
        properties = properties,
        packages = packages,
        label = label,
        man = man
      )
    },

    #' @description
    #' Performs the feature selection on a [FSelectInstanceBatchSingleCrit] or [FSelectInstanceBatchMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveBatchFSelect] that resides in the [FSelectInstanceBatchSingleCrit] / [FSelectInstanceBatchMultiCrit].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([FSelectInstanceBatchSingleCrit] | [FSelectInstanceBatchMultiCrit]).
    #'
    #' @return [data.table::data.table()].
    optimize = function(inst) {
      assert_fselect_instance_batch(inst)
      if ("requires_model" %in% self$properties) inst$objective$.__enclos_env__$private$.model_required = TRUE
      result = optimize_batch_default(inst, self)
      inst$objective$.__enclos_env__$private$.xss = NULL
      inst$objective$.__enclos_env__$private$.design = NULL
      inst$objective$.__enclos_env__$private$.benchmark_result = NULL
      inst$objective$.__enclos_env__$private$.aggregated_performance = NULL
      return(result)
    }
  )
)
