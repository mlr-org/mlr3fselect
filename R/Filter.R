#' @title Filter Base Class
#'
#' @description
#' FilterBase.
#'
#' @section Usage:
#' ```
#' Filter = Filter$new(id)
#' # public members
#' filter$id
#' filter$ff
#' filter$settings
#' filter$values
#' # public methods
#' filter$tune()
#' filter$tune_result()
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):\cr
#'   The id of the Filter
#' * `packages` (`character()`]:\cr
#'   Set of required packages.
#' * `supported_features` (`list`):\cr
#'   The feature types supported by the filter.
#' * `supported_tasks` (`list`):\cr
#'   The task types supported by the filter.
#' * `settings` (`list`):\cr
#'   The settings for the Filter
#'
#' @section Details:
#' * `$calculate(task)` calculates the filter values.
#' * `$new()` creates a new object of class [Filter].
#' * `$filter()` filters a [Task] using specific criteria
#'     -  returns a subsetted [Task]
#' * `$id` stores an identifier for this [Filter].
#' * `$filter_values` stores the calculated filter values.
#' * `$packages` stores the names of required packages.
#' * `$settings` is a list of hyperparamter settings for this [Filter].
#' @name Filter
#' @family Filter
NULL

#' @export
Filter = R6Class("Filter",
  public = list(
    id = NULL,
    packages = NULL,
    feature_types = NULL,
    task_type = NULL,
    settings = NULL,
    filter_values = NULL,

    initialize = function(id, packages, feature_types, task_type, settings) {
      self$id = assert_string(id)
      self$packages = assert_character(packages)
      self$feature_types = assert_character(feature_types)
      self$task_type = assert_character(task_type)
      self$settings = assert_list(settings, names = "unique")
    },

    calculate = function(task, settings = self$settings) {
      assert_task(task)
      assert_feature_types(task, self)
      assert_filter(self, task)
      assert_list(settings, names = "unique")
      require_namespaces(self$packages)


      fv = private$.calculate(task, settings)
      self$filter_values = sort(fv, decreasing = TRUE, na.last = TRUE)
      self
    },

    filter_abs = function(task, abs) {
      assert_count(abs)
      assert_task(task)
      if (is.null(self$filter_values))
        stopf("Filter values have not been computed yet")
      filter_n(self, task, abs)
    },

    filter_perc = function(task, perc) {
      assert_number(perc, lower = 0, upper = 1)
      assert_task(task)
      if (is.null(self$filter_values))
        stopf("Filter values have not been computed yet")
      filter_n(self, task, round(task$nrow * perc))
    },

    filter_thres = function(task, threshold) {
      assert_number(threshold)
      assert_task(task)
      if (is.null(self$filter_values))
        stopf("Filter values have not been computed yet")
      filter_n(self, task, sum(self$filter_values > threshold))
    }
  )
)

filter_n = function(self, task, n) {
  filtered_features = names(head(self$filter_values, n))
  task$clone(deep = TRUE)$select(filtered_features)
}

#' @export
as.data.table.Filter = function(x, ...) {
  fv = x$filter_values
  if (is.null(fv))
    stopf("No filter data available")
  enframe(x$filter_values)
}
