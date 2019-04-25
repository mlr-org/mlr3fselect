context("FilterVariableImportance")

test_that("FilterVariableImportance", {
  task = mlr3::mlr_tasks$get("wine")
  learner = mlr3::mlr_learners$get("classif.rpart")
  filter = FilterVariableImportance$new(learner = learner)
  filter$calculate(task)
  fn = task$feature_names
  expect_numeric(filter$scores, len = length(fn), any.missing = FALSE, names = "unique")
  expect_names(names(filter$scores), permutation.of = fn)
})
