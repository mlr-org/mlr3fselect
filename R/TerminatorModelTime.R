#' @title Terminator that stops after a budget of model evaluation time is depleted
#'
#' @aliases mlr_terminators_model_time
#' @include Terminator.R
#'
#' @description
#' Class to terminate the optimization after a given model evaluation budget is exceeded.
#' The terminator measures the used time to train and predict all models contained
#' in the archive.
#'
#' @section Parameters:
#' * `secs` :: `numeric(1)`\cr
#'   Maximum allowed time, in seconds, default is 0.
#'
#' @export
#' @examples
#' TerminatorModelTime$new()
#' term("model_time", secs = 10 * 3600)
TerminatorModelTime = R6Class("TerminatorModelTime",
  inherit = Terminator,
  public = list(

    #' @description
    #' Create new `TerminatorModelTime` object.
    #' @return A `TerminatorModelTime` object.
    initialize = function() {
      ps = ParamSet$new(list(ParamDbl$new("secs", lower = 0, default = 0, tags = "required")))
      ps$values = list(secs = 0)
      super$initialize(param_set = ps)
    },

    #' @description
    #' Is `TRUE` if after a given model evaluation budget is exceeded, and `FALSE` otherwise.
    #' @param instance object of class [FSelect] or [Tuner].
    #' @return `logical(1)`
    is_terminated = function(instance) {
      if (is.null(instance$bmr))
        return(FALSE)
      # extract train and predict timings and sum them up
      t_all = sum(map_dbl(instance$bmr$data$learner, function(x) sum(x$timings)))
      return(t_all >= self$param_set$values$secs)
    }
  )
)

mlr_terminators$add("model_time", TerminatorModelTime)
