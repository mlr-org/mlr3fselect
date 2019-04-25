assert_filter = function (filter, task = NULL) {
  assert_class(filter, "Filter")
  if (!is.null(task)) {
    if (task$task_type %nin% filter$task_type) {
      stopf("Filter '%s' is not compatible with type of task '%s' (type: %s)",
            filter$id, task$id, task$task_type)
    }
  }
  invisible(filter)
}
