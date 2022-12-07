#' @title Backup Benchmark Result Callback
#'
#' @include CallbackFSelect.R
#' @name mlr3fselect.backup
#'
#' @description
#' This [CallbackFSelect] writes the [mlr3::BenchmarkResult] after each batch to disk.
#'
#' @examples
#' clbk("mlr3fselect.backup", path = "backup.rds")
#'
#' # Run feature selection on the Palmer Penguins data set
#' instance = fselect(
#'   method = "random_search",
#'   task = tsk("pima"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp ("holdout"),
#'   measures = msr("classif.ce"),
#'   term_evals = 4,
#'   callbacks = clbk("mlr3fselect.backup", path = tempfile(fileext = ".rds")))
NULL

load_callback_backup = function() {
  callback_fselect("mlr3fselect.backup",
    label = "Backup Benchmark Result Callback",
    man = "mlr3fselect::mlr3fselect.backup",
    on_optimization_begin = function(callback, context) {
      if (is.null(callback$state$path)) callback$state$path = "bmr.rds"
      assert_path_for_output(callback$state$path)
    },

    on_optimizer_after_eval = function(callback, context) {
      if (file.exists(callback$state$path)) unlink(callback$state$path)
      saveRDS(context$instance$archive$benchmark_result, callback$state$path)
    }
  )
}
