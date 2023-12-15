expect_fselector = function(fselector) {
  expect_r6(fselector, "FSelector",
    public = c("optimize", "param_set", "properties", "packages"),
    private = c(".optimize", ".assign_result"))
}

expect_best_features = function(res, features) {
  expect_set_equal(names(res)[as.logical(res)], features)
}

expect_feature_number = function(features, n) {
  res = rowSums(features)
  expect_set_equal(res, n)
}

expect_max_features = function(features, n) {
  res = max(rowSums(features))
  expect_set_equal(res, n)
}

expect_features = function(res, identical_to = NULL, must_include = NULL) {
  expect_names(names(res)[as.logical(res)], must.include = must_include, identical.to = identical_to)
}
