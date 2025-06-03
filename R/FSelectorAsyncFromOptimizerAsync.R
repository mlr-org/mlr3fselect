#' @title FSelectorAsyncFromOptimizerAsync
#'
#' @description
#' Internally used to transform [bbotk::Optimizer] to [FSelector].
#'
#' @template param_man
#'
#' @keywords internal
#' @export
FSelectorAsyncFromOptimizerAsync = R6Class("FSelectorAsyncFromOptimizerAsync",
  inherit = FSelectorAsync,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param optimizer [bbotk::Optimizer]\cr
    #' Optimizer that is called.
    initialize = function(optimizer, man = NA_character_) {
      private$.optimizer = assert_optimizer_async(optimizer)
      packages = union("mlr3fselect", optimizer$packages)
      assert_string(man, na.ok = TRUE)

      super$initialize(
        id = if ("id" %in% names(optimizer)) optimizer$id else "fselector",
        param_set = optimizer$param_set,
        properties = optimizer$properties,
        packages = packages,
        label = optimizer$label,
        man = man
      )
    },

    #' @description
    #' Performs the feature selection on a [FSelectInstanceAsyncSingleCrit] /
    #' [FSelectInstanceAsyncMultiCrit] until termination. The single evaluations and
    #' the final results will be written into the [ArchiveAsyncFSelect] that
    #' resides in the [FSelectInstanceAsyncSingleCrit]/[FSelectInstanceAsyncMultiCrit].
    #' The final result is returned.
    #'
    #' @param inst ([FSelectInstanceAsyncSingleCrit] | [FSelectInstanceAsyncMultiCrit]).
    #'
    #' @return [data.table::data.table].
    optimize = function(inst) {
      assert_fselect_instance_async(inst)
      private$.optimizer$optimize(inst)
    }
  ),

  active = list(

    #' @field param_set ([paradox::ParamSet])\cr
    #' Set of control parameters.
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.optimizer$param_set)) {
        stop("$param_set is read-only.")
      }
      private$.optimizer$param_set
    }
  ),

  private = list(
    .optimizer = NULL
  )
)


