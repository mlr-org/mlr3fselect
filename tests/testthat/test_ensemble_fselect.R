test_that("ensemble feature selection works", {
  res = ensemble_fselect(
    fselector = fs("rfe", n_features = 2, feature_fraction = 0.8),
    task = tsk("sonar"),
    learners = lrns(c("classif.rpart", "classif.featureless")),
    init_resampling = rsmp("subsampling", repeats = 2),
    inner_resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("none")
  )

  expect_data_table(res, nrows = 4)
  expect_list(res$features, any.missing = FALSE, len = 4)
  expect_vector(res$n_features, size = 4)
  expect_vector(res$classif.ce, size = 4)
  expect_list(res$importance, any.missing = FALSE, len = 4)
})


