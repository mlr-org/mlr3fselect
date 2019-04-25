assert_filter = function (filter, task = NULL) {
  assert_class(filter, "Filter")
  if (!is.null(task)) {
    if (!any(task$task_type %in% filter$task_type)) {
      stopf("Filter '%s' is not compatible with type of task '%s' (type: %s)",
            filter$id, task$id, task$task_type)
    }
  }
  invisible(filter)
}
