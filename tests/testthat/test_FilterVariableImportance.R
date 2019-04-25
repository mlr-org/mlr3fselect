context("FilterVariableImportance")

test_that("FilterVariableImportance", {
  task = mlr3::mlr_tasks$get("wine")
  learner = mlr3::mlr_learners$get("classif.rpart")
  f = FilterVariableImportance$new(learner = learner)
  f$calculate(task)
  fn = task$feature_names
  expect_numeric(f$scores, len = length(fn), any.missing = FALSE, names = "unique")
  expect_names(names(f$scores), permutation.of = fn)
})
