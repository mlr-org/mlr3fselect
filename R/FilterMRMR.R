#' @title Minimum redundancy, maximum relevance filter
#'
#' @description
#' Minimum redundancy, maximum relevance filter
#'
#' @section Usage:
#' ```
#' filter = FilterMRMR$new()
#' ```
#'
#' @inheritSection Filter Details
#' @section Details:
#' `$new()` creates a new object of class [FilterMRMR].
#'
#' @name FilterMRMR
#' @family Filter
#' @examples
#' task = mlr_tasks$get("trees")
#' filter = FilterMRMR$new()
#' filter$calculate(task)
NULL

#' @export
#' @include Filter.R
FilterMRMR = R6Class("FilterMRMR", inherit = Filter,
  public = list(
    initialize = function(id = "FilterMRMR", settings = list()) {
      super$initialize(
        id = id,
        packages = "mRMRe",
        feature_types = c("numeric", "ordered"),
        task_type = "regr",
        settings = settings)
    },
    calculate = function(task, settings = self$settings) {

      # check for supported features
      assert_feature_types(task, self)
      # check for supported task
      assert_filter(filter, task)

      # check for Namespace
      require_namespaces(self$packages)

      # assign task to class
      self$task = task

      browser()

      threads.before = mRMRe::get.thread.count()
      on.exit(mRMRe::set.thread.count(threads.before))
      mRMRe::set.thread.count(1L)
      data = mRMRe::mRMR.data(data = task$data())
      target.ind = match(task$target_names, colnames(task$data()))

      filter_values = invoke(mRMRe::mRMR.classic,
        data = data, target_indices = target.ind,
        .args = settings)

      ### ERROR here
      scores = as.numeric(mRMRe::scores(filter_values)[[1L]])
      setNames(scores, filter_values@feature_names[as.integer(mRMRe::solutions(filter_values)[[1L]])])



      self$filter_values = sort(filter_values, decreasing = TRUE,
        na.last = TRUE)
    }
  )
)

