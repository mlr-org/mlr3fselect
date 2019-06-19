expect_filter_result = function(fr) {

  checkmate::expect_r6(fr, "FilterResult", public = c("scores"))
  testthat::expect_output(print(fr), "FilterResult")

  checkmate::expect_data_table(fr$scores)
  checkmate::expect_names(names(fr$scores), must.include = c("method", "scores"))

  tab = fr$feature_types
  checkmate::expect_character(tab)

  tab = fr$packages
  checkmate::expect_character(tab)

  tab = fr$task_type
  checkmate::expect_character(tab)

  tab = fr$task_properties
  checkmate::expect_character(tab)

}
