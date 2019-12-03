#' FSelect
#'
#' @include mlr_fselectors.R
#'
#' @description
#' Abstract `FSelect` class that implements the base functionality each `FSelect*` class must provide.
#'
#' @export
FSelect = R6Class("FSelect",
  public = list(
    #' @field param_set [paradox::ParamSet]
    param_set = NULL,

    #' @description
    #' Create new `FSelect` object.
    #' @param param_set [paradox::ParamSet]
    #' Set of control parameter for the feature selection.
    #' @return A `FSelect` object.
    initialize = function(param_set) {
      self$param_set = assert_param_set(param_set)
    },

    #' @description
    #' Performce the feature selection on a [FSelectInstance] until termination.
    #' @param instance [FSelectInstance]
    select = function(instance) {
      tryCatch({
        while (TRUE) {
          private$select_internal(instance)
        }
      }, terminated_error = function(cond) {})

      private$assign_result(instance)
      invisible(NULL)
    }
  ),

  private = list(
    select_internal = function() {
      # Implemented by subclass
      stop("Abstract")
    },

    assign_result = function(instance) {
      fselect_assign_result_default(instance)
    }
  )
)

fselect_assign_result_default = function(instance) {
  assert_r6(instance, "FSelectInstance")

  rr = instance$best()
  perf = rr$aggregate(instance$measures[[1]])
  feat = instance$bmr$rr_data[rr$uhash, on = "uhash"]$feat[[1]]

  instance$assign_result(feat, perf)
  invisible(NULL)
}
