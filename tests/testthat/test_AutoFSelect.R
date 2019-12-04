context("AutoFSelect")

test_that("AutoFSelect / train+predict", {
  te = term("evals", n_evals = 10)
  task = tsk("iris")
  ms = MeasureDummyCPClassif$new()
  fs = fs("sequential")
  at = AutoFSelect$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, fselect = fs)
  expect_learner(at)
  at$train(task)
  expect_learner(at)
  inst = at$fselect_instance
  expect_benchmark_result(inst$bmr)
  a = at$archive()
  expect_data_table(a, nrows = 10L)
  r = at$fselect_result
  expect_equal(r$feat, c("Petal.Length", "Petal.Width", "Sepal.Length"))
  prd = at$predict(task)
  expect_prediction(prd)
  expect_is(at$learner$model, "rpart")
})
