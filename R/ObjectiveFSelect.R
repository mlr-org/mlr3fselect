#' @export
ObjectiveFSelect = R6Class("ObjectiveFSelect",
  inherit = Objective,
  public = list(
    fselectinstance = NULL,

    eval_many = function(xss) {
      tasks = map(xss, function(x) {
        state = self$fselectinstance$task$feature_names[unlist(x)]
        tsk = self$fselectinstance$task$clone(deep = TRUE)
        tsk$select(state)
        return(tsk)
      })

      design = benchmark_grid(tasks, self$fselectinstance$learner,
        self$fselectinstance$resampling)
      bmr = benchmark(design, store_models = self$fselectinstance$store_models)
      bmr_data = split(bmr$data, by = "uhash")
      aggr = bmr$aggregate(self$fselectinstance$measures)

      y = map_chr(self$fselectinstance$measures, function(s) s$id)

      cbind(aggr[, y, with = FALSE], bmr_data = bmr_data)
    }
  )
)
