#' @title Create Feature Selection Callback
#'
#' @description
#' Specialized [bbotk::CallbackBatch] for feature selection.
#' Callbacks allow customizing the behavior of processes in mlr3fselect.
#' The [callback_batch_fselect()] function creates a [CallbackBatchFSelect].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#' For more information on callbacks see [callback_batch_fselect()].
#'
#' @examples
#' # Write archive to disk
#' callback_batch_fselect("mlr3fselect.backup",
#'   on_optimization_end = function(callback, context) {
#'     saveRDS(context$instance$archive, "archive.rds")
#'   }
#' )
CallbackBatchFSelect = R6Class("CallbackBatchFSelect",
  inherit = CallbackBatch,
  public = list(

    #' @field on_eval_after_design (`function()`)\cr
    #'   Stage called after design is created.
    #'   Called in `ObjectiveFSelectBatch$eval_many()`.
    on_eval_after_design = NULL,

    #' @field on_eval_after_benchmark (`function()`)\cr
    #'   Stage called after feature sets are evaluated.
    #'   Called in `ObjectiveFSelectBatch$eval_many()`.
    on_eval_after_benchmark = NULL,

    #' @field on_eval_before_archive (`function()`)\cr
    #'   Stage called before performance values are written to the archive.
    #'   Called in `ObjectiveFSelectBatch$eval_many()`.
    on_eval_before_archive = NULL
  )
)

#' @title Create Feature Selection Callback
#'
#' @description
#' Function to create a [CallbackBatchFSelect].
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
#' A feature selection callback works with [bbotk::ContextBatch] and [ContextBatchFSelect].
#'
#' @details
#' When implementing a callback, each function must have two arguments named `callback` and `context`.
#'
#' A callback can write data to the state (`$state`), e.g. settings that affect the callback itself.
#' Avoid writing large data the state.
#' This can slow down the feature selection when the evaluation of configurations is parallelized.
#'
#' Feature selection callbacks access two different contexts depending on the stage.
#' The stages `on_eval_after_design`, `on_eval_after_benchmark`, `on_eval_before_archive` access [ContextBatchFSelect].
#' This context can be used to customize the evaluation of a batch of feature sets.
#' Changes to the state of callback are lost after the evaluation of a batch and changes to the fselect instance or the fselector are not possible.
#' Persistent data should be written to the archive via `$aggregated_performance` (see [ContextBatchFSelect]).
#' The other stages access [ContextBatch].
#' This context can be used to modify the fselect instance, archive, fselector and final result.
#' There are two different contexts because the evaluation can be parallelized i.e. multiple instances of [ContextBatchFSelect] exists on different workers at the same time.
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
#' @param on_optimizer_before_eval (`function()`)\cr
#'   Stage called after the optimizer proposes points.
#'   Called in `OptimInstance$eval_batch()`.
#' @param on_eval_after_design (`function()`)\cr
#'   Stage called after design is created.
#'   Called in `ObjectiveFSelectBatch$eval_many()`.
#' @param on_eval_after_benchmark (`function()`)\cr
#'   Stage called after feature sets are evaluated.
#'   Called in `ObjectiveFSelectBatch$eval_many()`.
#' @param on_eval_before_archive (`function()`)\cr
#'   Stage called before performance values are written to the archive.
#'   Called in `ObjectiveFSelectBatch$eval_many()`.
#' @param on_optimizer_after_eval (`function()`)\cr
#'   Stage called after points are evaluated.
#'   Called in `OptimInstance$eval_batch()`.
#' @param on_result (`function()`)\cr
#'   Stage called after result are written.
#'   Called in `OptimInstance$assign_result()`.
#' @param on_optimization_end (`function()`)\cr
#'   Stage called at the end of the optimization.
#'   Called in `Optimizer$optimize()`.
#'
#' @export
#' @inherit CallbackBatchFSelect examples
callback_batch_fselect = function(
  id,
  label = NA_character_,
  man = NA_character_,
  on_optimization_begin = NULL,
  on_optimizer_before_eval = NULL,
  on_eval_after_design = NULL,
  on_eval_after_benchmark = NULL,
  on_eval_before_archive = NULL,
  on_optimizer_after_eval = NULL,
  on_result = NULL,
  on_optimization_end = NULL
  ) {
  stages = discard(set_names(list(
    on_optimization_begin,
    on_optimizer_before_eval,
    on_eval_after_design,
    on_eval_after_benchmark,
    on_eval_before_archive,
    on_optimizer_after_eval,
    on_result,
    on_optimization_end),
    c(
      "on_optimization_begin",
      "on_optimizer_before_eval",
      "on_eval_after_design",
      "on_eval_after_benchmark",
      "on_eval_before_archive",
      "on_optimizer_after_eval",
      "on_result",
      "on_optimization_end")), is.null)
  walk(stages, function(stage) assert_function(stage, args = c("callback", "context")))
  callback = CallbackBatchFSelect$new(id, label, man)
  iwalk(stages, function(stage, name) callback[[name]] = stage)
  callback
}
