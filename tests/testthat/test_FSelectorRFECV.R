test_that("extra columns are stored in the archive", {
  instance = fsi(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("dummy"),
    terminator = trm("none"),
    store_models = TRUE
  )

  optimizer = fs("rfecv", n_features = 1, feature_number = 1)
  optimizer$optimize(instance)

  expect_names(names(instance$archive$data), must.include = c("importance", "iteration"))
})

test_that("resampling is converted", {
  task = TEST_MAKE_TSK()
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  instance = fsi(
    task = task,
    learner = lrn("regr.rpart"),
    resampling = resampling,
    measures = msr("dummy"),
    terminator = trm("none"),
    store_models = TRUE
  )

  optimizer = fs("rfecv", n_features = 1, feature_number = 1)
  optimizer$optimize(instance)
  data = as.data.table(instance$archive)

  # check cv to custom
  walk(seq(3), function(i) {
    walk(data[list(i), resample_result, on = "iteration"], function(rr) {
      expect_class(rr$resampling, "ResamplingCustom")
      expect_equal(rr$resampling$train_set(1), resampling$train_set(i))
      expect_equal(rr$resampling$test_set(1), resampling$test_set(i))
    })
  })

  # check final run
  walk(data[is.na(iteration), resample_result], function(rr) {
    expect_class(rr$resampling, "ResamplingInsample")
  })
})

test_that("default parameters work", {
  instance = fsi(
    task = TEST_MAKE_TSK(),
    learner = lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("dummy"),
    terminator = trm("none"),
    store_models = TRUE
  )

  optimizer = fs("rfecv")
  optimizer$optimize(instance)
  data = as.data.table(instance$archive)

  walk(seq(3), function(i) {
    data = data[list(i), , on = "iteration"]
    expect_feature_number(data[1, 1:4], n = 4)
    expect_feature_number(data[2, 1:4], n = 2)
  })
})

test_that("learner without importance method throw an error", {
  learner = lrn("classif.rpart")
  learner$properties = setdiff(learner$properties, "importance")

  expect_error(fselect(
    method = fs("rfecv"),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    store_models = TRUE
  ), "does not work with")
})
