context("FilterEmbedded")

test_that("FilterEmbedded", {
  task = mlr3::mlr_tasks$get("wine")
  learner = mlr3::mlr_learners$get("classif.rpart")
  f = FilterEmbedded$new(learner = learner)
  f$calculate(task)
  fn = task$feature_names
  expect_data_table(f$scores)
  expect_names(f$scores$feature, permutation.of = fn)
})
