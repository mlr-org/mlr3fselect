#' @title Feature Selection with Asynchronous Design Points
#'
#' @name mlr_fselectors_async_design_points
#'
#' @description
#' Subclass for asynchronous design points feature selection.
#'
#' @templateVar id async_design_points
#' @template section_dictionary_fselectors
#'
#' @inheritSection bbotk::OptimizerAsyncDesignPoints Parameters
#'
#' @family FSelectorAsync
#' @export
FSelectorAsyncDesignPoints = R6Class("FSelectorAsyncDesignPoints",
  inherit = FSelectorAsyncFromOptimizerAsync,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerAsyncDesignPoints$new(),
        man = "mlr3fselect::mlr_fselectors_async_design_points"
      )
    }
  )
)

mlr_fselectors$add("async_design_points", FSelectorAsyncDesignPoints)
