#' FSelectExhaustive Class
#'
#' @description
#' Exhaustive feature selection.
#'
#' @export
FSelectExhaustive = R6Class("FSelectExhaustive",
  inherit = FSelect,
  public = list(
    #' @description
    #' Create new `FSelectExhaustive` object.
    #' @return A `FSelectExhaustive` object.
    initialize = function() {
      ps = ParamSet$new(list(
         ParamInt$new("max_features", lower = 1))
      )

    super$initialize(
       param_set = ps
     )
   }
  ),
  private = list(
   select_internal = function(instance) {
     pars = self$param_set$values

     if(length(instance$bmr$rr_data$batch_n) == 0) f_nr = 1 else f_nr = instance$bmr$rr_data$batch_n[length(instance$bmr$rr_data$batch_n)] + 1

     if (f_nr > pars$max_features) {
       stop(terminated_error(instance))
     }

     combinations = combn(length(instance$task$feature_names), f_nr)
     states = t(sapply(seq_len(ncol(combinations)), function(j) {
        state = rep(0, length(instance$task$feature_names))
        state[combinations[, j]] = 1
        state
     }))

     instance$eval_batch(states)
   },

   set_defaults = function(instance) {
     if (is.null(self$param_set$values$max_features)) self$param_set$values$max_features = length(instance$task$feature_names)
   }
  )
)

mlr_fselectors$add("exhaustive", FSelectExhaustive)
