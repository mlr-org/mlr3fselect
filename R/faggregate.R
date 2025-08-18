#' @title Fast Aggregation of ResampleResults and BenchmarkResults
#'
#' @description
#' Aggregates a [mlr3::ResampleResult] or [mlr3::BenchmarkResult] for a single simple measure.
#' Returns the aggregated score for each resample result.
#'
#' @details
#' This function is faster than `$aggregate()` because it does not reassemble the resampling results.
#' It only works on simple measures which do not require the task, learner, model or train set to be available.
#'
#' @param obj ([mlr3::ResampleResult] | [mlr3::BenchmarkResult]).
#' @param measure ([mlr3::Measure]).
#' @param conditions (`logical(1)`)\cr
#' If `TRUE`, the function returns the number of warnings and the number of errors.
#'
#' @return ([data.table::data.table()])
#'
#' @export
faggregate = function(obj, measure, conditions = FALSE) {
  tab = fscore(obj, measure, conditions = conditions)
  aggregator = measure$aggregator %??% mean
  if (conditions) {
    set_names(tab[, list(
     score = aggregator(get(measure$id)),
     warnings = sum(warnings),
     errors = sum(errors)),
     by = "uhash"], c("uhash", measure$id, "warnings", "errors"))[, -c("uhash"), with = FALSE]
  } else {
    set_names(tab[, list(score = aggregator(get(measure$id))), by = "uhash"], c("uhash", measure$id))[, -c("uhash"), with = FALSE]
  }
}

fscore = function(obj, measure, conditions = FALSE) {
  data = get_private(obj)$.data$data
  # sort by uhash
  tab = data$fact[data$uhashes, c("iteration", "prediction", "uhash", "learner_state"), with = FALSE]
  set(tab, j = measure$id, value = map_dbl(tab$prediction, fscore_single_measure, measure = measure))
  cns = c("uhash", measure$id)
  if (conditions) {
    set(tab, j = "warnings", value = map_int(tab$learner_state, function(s) sum(s$log$class == "warning")))
    set(tab, j = "errors", value = map_int(tab$learner_state, function(s) sum(s$log$class == "error")))
    cns = c(cns, "warnings", "errors")
  }
  tab[, cns, with = FALSE]
}

fscore_single_measure = function(prediction, measure) {
  # no predict sets
  if (!length(measure$predict_sets)) {
    score = get_private(measure)$.score(prediction = NULL, task = NULL)
    return(score)
  }

  # merge multiple predictions (on different predict sets) to a single one
  if (is.list(prediction)) {
    ii = match(measure$predict_sets, names(prediction))
    if (anyMissing(ii)) {
      return(NaN)
    }
    prediction = do.call(c, prediction[ii])
  }

  # convert pdata to regular prediction
  prediction = as_prediction(prediction, check = FALSE)

  if (is.null(prediction) && length(measure$predict_sets)) {
    return(NaN)
  }

  if (!is_scalar_na(measure$predict_type) && measure$predict_type %nin% prediction$predict_types) {
    return(NaN)
  }

  get_private(measure)$.score(prediction = prediction, task = NULL, weights = if (measure$use_weights == "use") prediction$weights)
}
