#' @title Create Feature Selection Callback
#'
#' @description
#' Specialized [bbotk::CallbackOptimization] for feature selection.
#' Callbacks allow to customize the behavior of processes in mlr3fselect.
#' The [callback_fselect()] function creates a [CallbackFSelect].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#' For more information on callbacks see [callback_fselect()].
#'
#' @examples
#' # Write archive to disk
#' callback_fselect("mlr3fselect.backup",
#'   on_optimization_end = function(callback, context) {
#'     saveRDS(context$instance$archive, "archive.rds")
#'   }
#' )
CallbackFSelect = R6Class("CallbackFSelect",
  inherit = bbotk::CallbackOptimization,
  public = list(

    #' @field on_eval_after_design (`function()`)\cr
    #'   Stage called after design is created.
    #'   Called in `ObjectiveFSelect$eval_many()`.
    on_eval_after_design = NULL,

    #' @field on_eval_after_benchmark (`function()`)\cr
    #'   Stage called after feature sets are evaluated.
    #'   Called in `ObjectiveFSelect$eval_many()`.
    on_eval_after_benchmark = NULL,

    #' @field on_eval_before_archive (`function()`)\cr
    #'   Stage called before performance values are written to the archive.
    #'   Called in `ObjectiveFSelect$eval_many()`.
    on_eval_before_archive = NULL
  )
)

#' @title Create Feature Selection Callback
#'
#' @description
#' Function to create a [CallbackFSelect].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#'
#' Feature selection callbacks can be called from different stages of feature selection.
#' The stages are prefixed with `on_*`.
#'
#' ```
#' Start Feature Selection
#'      - on_optimization_begin
#'     Start FSelect Batch
#'          - on_optimizer_before_eval
#'         Start Evaluation
#'              - on_eval_after_design
#'              - on_eval_after_benchmark
#'              - on_eval_before_archive
#'         End Evaluation
#'          - on_optimizer_after_eval
#'     End FSelect Batch
#'      - on_result
#'      - on_optimization_end
#' End Feature Selection
#' ```
#'
#' See also the section on parameters for more information on the stages.
#' A feature selection callback works with [bbotk::ContextOptimization] and [ContextEval].
#'
#' @details
#' When implementing a callback, each functions must have two arguments named `callback` and `context`.
#'
#' A callback can write data to the state (`$state`), e.g. settings that affect the callback itself.
#' Avoid writing large data the state.
#' This can slow down the feature selection when the evaluation of configurations is parallelized.
#'
#' Feature selection callbacks access two different contexts depending on the stage.
#' The stages `on_eval_after_design`, `on_eval_after_benchmark`, `on_eval_before_archive` access [ContextEval].
#' This context can be used to customize the evaluation of a batch of feature sets.
#' Changes to the state of callback are lost after the evaluation of a batch and changes to the fselect instance or the fselector are not possible.
#' Persistent data should be written to the archive via `$aggregated_performance` (see [ContextEval]).
#' The other stages access [ContextOptimization].
#' This context can be used to modify the fselect instance, archive, fselector and final result.
#' There are two different contexts because the evaluation can be parallelized i.e. multiple instances of [ContextEval] exists on different workers at the same time.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param label (`character(1)`)\cr
#'   Label for the new instance.
#' @param man (`character(1)`)\cr
#'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'   The referenced help package can be opened via method `$help()`.
#' @param on_optimization_begin (`function()`)\cr
#'   Stage called at the beginning of the optimization.
#'   Called in `Optimizer$optimize()`.
#'   The context available is [bbotk::ContextOptimization].
#' @param on_optimizer_before_eval (`function()`)\cr
#'   Stage called after the optimizer proposes points.
#'   Called in `OptimInstance$eval_batch()`.
#'   The context available is [bbotk::ContextOptimization].
#' @param on_eval_after_design (`function()`)\cr
#'   Stage called after design is created.
#'   Called in `ObjectiveFSelect$eval_many()`.
#'   The context available is [ContextEval].
#' @param on_eval_after_benchmark (`function()`)\cr
#'   Stage called after feature sets are evaluated.
#'   Called in `ObjectiveFSelect$eval_many()`.
#'   The context available is [ContextEval].
#' @param on_eval_before_archive (`function()`)\cr
#'   Stage called before performance values are written to the archive.
#'   Called in `ObjectiveFSelect$eval_many()`.
#'   The context available is [ContextEval].
#' @param on_optimizer_after_eval (`function()`)\cr
#'   Stage called after points are evaluated.
#'   Called in `OptimInstance$eval_batch()`.
#'   The context available is [bbotk::ContextOptimization].
#' @param on_result (`function()`)\cr
#'   Stage called after result are written.
#'   Called in `OptimInstance$assign_result()`.
#'   The context available is [bbotk::ContextOptimization].
#' @param on_optimization_end (`function()`)\cr
#'   Stage called at the end of the optimization.
#'   Called in `Optimizer$optimize()`.
#'   The context available is [bbotk::ContextOptimization].
#'
#' @export
#' @inherit CallbackFSelect examples
callback_fselect = function(id, label = NA_character_, man = NA_character_, on_optimization_begin = NULL, on_optimizer_before_eval = NULL, on_eval_after_design = NULL, on_eval_after_benchmark = NULL, on_eval_before_archive = NULL, on_optimizer_after_eval = NULL, on_result = NULL,  on_optimization_end = NULL) {
  stages = discard(set_names(list(on_optimization_begin, on_optimizer_before_eval, on_eval_after_design, on_eval_after_benchmark, on_eval_before_archive, on_optimizer_after_eval, on_result, on_optimization_end), c("on_optimization_begin", "on_optimizer_before_eval", "on_eval_after_design", "on_eval_after_benchmark", "on_eval_before_archive", "on_optimizer_after_eval", "on_result",  "on_optimization_end")), is.null)
  walk(stages, function(stage) assert_function(stage, args = c("callback", "context")))
  callback = CallbackFSelect$new(id, label, man)
  iwalk(stages, function(stage, name) callback[[name]] = stage)
  callback
}
