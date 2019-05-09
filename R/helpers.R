assert_filter = function(filter, task = NULL) {
  filter = cast_from_dict(filter, "Filter", mlr_filters, FALSE)[[1L]]

  if (!is.null(task)) {
    if (task$task_type %nin% filter$task_type) {
      stopf("Filter '%s' is not compatible with type of task '%s' (type: %s)",
        filter$id, task$id, task$task_type)
    }
  }
  return(filter)
}
