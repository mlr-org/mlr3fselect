#' @title Class for Asynchronous Feature Selection Algorithms
#'
#' @include mlr_fselectors.R
#'
#' @description
#' The [FSelectorAsync] implements the asynchronous optimization algorithm.
#'
#' @details
#' [FSelectorAsync] is an abstract base class that implements the base functionality each asynchronous fselector must provide.
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
FSelectorAsync = R6Class("FSelectorAsync",
  inherit = FSelector,
  public = list(

    #' @description
    #' Performs the feature selection on a [FSelectInstanceAsyncSingleCrit] or [FSelectInstanceAsyncMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveAsyncFSelect] that resides in the [FSelectInstanceAsyncSingleCrit]/[FSelectInstanceAsyncMultiCrit].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([FSelectInstanceAsyncSingleCrit] | [FSelectInstanceAsyncMultiCrit]).
    #'
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      assert_fselect_instance_async(inst)
      optimize_async_default(inst, self)
    }
  )
)
