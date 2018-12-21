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
#' * `$filterFeatures()` filters a [Task] using specific criteria
#'     -  returns a subsetted [Task]
#' * `$id` stores an identifier for this [Filter].
#' * `$filterValues` stores the calculated filter values.
#' * `$packages` stores the names of required packages.
#' * `$settings` is a list of hyperparamter settings for this [Filter].
#' @name Filter
#' @family Filter
#' @examples
#' filter = Filter$new(id = "FilterLinearCorrelation", package =  "stats",
#'   feature_types = "numeric", task_type = "regr", settings = list())
#'
NULL

#' @export
Filter = R6Class("Filter",
  public = list(
    id = NULL,
    task = NULL,
    packages = NULL,
    feature_types = NULL,
    task_type = NULL,
    settings = NULL,
    filter_values = NULL,
    filtered_task = NULL,

    initialize = function(id, packages, feature_types, task_type, settings) {
      self$id = assert_string(id)
      self$packages = assert_character(packages)
      self$feature_types = assert_character(feature_types)
      self$task_type = assert_character(task_type)
      self$settings = assert_list(settings, names = "unique")
    },
    filter = function(abs, perc, threshold) {
      assert_numeric(self$filter_values)

      self$filter_values = sort(self$filter_values, decreasing = TRUE)

      if (abs) {
        subs = abs
      } else if (perc) {
        subs = round(length(task$feature_names) * perc)
      } else if (threshold) {
        subs = length(which(self$filter_values > threshold))
      }

      filtered_features = names(self$filter_values[1:subs])
      self$filtered_task = task$select(filtered_features)
    }
  )
)
