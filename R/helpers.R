assert_feature_types = function(task, object) {
  features = unique(task$feature_types$type)
  features_unsupp = setdiff(features, object$feature_types)

  if (length(features_unsupp) > 0L) {
    stopf("Filter '%s' does not support %s features.",
          object$id,  str_collapse(features_unsupp, sep  = " and ", quote = "'"))
  }
}

assert_filter = function (filter, task = NULL)
{
  assert_class(filter, "Filter")
  if (!is.null(task)) {
    if (!any(task$task_type %in% filter$task_type)) {
      stopf("Filter '%s' is not compatible with type of task '%s' (type: %s)",
            filter$id, task$id, task$task_type)
    }
  }
  invisible(filter)
}

measureAUC = function(probabilities, truth, negative, positive) {
  if (is.factor(truth)) {
    i = as.integer(truth) == which(levels(truth) == positive)
  } else {
    i = truth == positive
  }
  if (length(unique(i)) < 2L) {
    stop("truth vector must have at least two classes")
  }
  #Use fast ranking function from data.table for larger vectors
  if (length(i) > 5000L) {
    r = frankv(probabilities)
  } else {
    r = rank(probabilities)
  }
  n.pos = as.numeric(sum(i))
  n.neg = length(i) - n.pos
  (sum(r[i]) - n.pos * (n.pos + 1) / 2) / (n.pos * n.neg)
}
