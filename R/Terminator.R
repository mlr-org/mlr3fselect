#' Abstract Terminator Class
#'
#' @include mlr_terminators.R
#'
#' @description
#' Abstract `Terminator` class that implements the base functionality each terminator must provide.
#' A terminator is an object that determines when to stop the optimization.
#'
#' Termination of optimization works as follows:
#' * Evaluations in an optimization are performed in batches.
#' * Before and after each batch evaluation, the [Terminator] is checked, and if it is positive, we stop.
#' * The optimization algorithm itself might decide not to produce any more points, or even might decide to do a smaller batch in its last evaluation.
#'
#' Therefore the following note seems in order:
#' While it is definitely possible to execute a fine-grained control for termination, and for many optimization we can specify exactly when to stop,
#' it might happen that too few or even too many evaluations are performed, especially if multiple points are evaluated in a single batch.
#' So it is advised to check the size of the returned archive, in particular if you are benchmarking multiple optimization algorithms.
#'
#' @export
Terminator = R6Class("Terminator",
  public = list(
    #' @field param_set [paradox::ParamSet]
    param_set = NULL,

    #' @description
    #' Create new `Terminator` object.
    #' @param param_set [paradox::ParamSet]
    #' Set of control parameters for terminator.
    #' @return A `Terminator` object.
    initialize = function(param_set = ParamSet$new()) {
      self$param_set = assert_param_set(param_set)
    },

    #' @description
    #' Format method.
    #' @return `character`
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Print method.
    #' @return `character`
    print = function() {
      catf(self$format())
      catf(str_indent("* Parameters:", as_short_string(self$param_set$values)))
    },

    #' @description
    #' Is `TRUE` if the termination criterion is positive, and `FALSE` otherwise.
    #' Must be implemented in each subclass.
    #' @param instance object of class [FSelect].
    #' @return `logical(1)`
    is_terminated = function(instance) TRUE # overwrite in subclasses
  )
)
