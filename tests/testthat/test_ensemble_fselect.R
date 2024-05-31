test_that("ensemble feature selection works", {
  efsr = ensemble_fselect(
    fselector = fs("rfe", n_features = 2, feature_fraction = 0.8),
    task = tsk("sonar"),
    learners = lrns(c("classif.rpart", "classif.featureless")),
    init_resampling = rsmp("subsampling", repeats = 2),
    inner_resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("none")
  )

  expect_data_table(efsr$grid, nrows = 4)
  expect_list(efsr$grid$features, any.missing = FALSE, len = 4)
  expect_vector(efsr$grid$n_features, size = 4)
  expect_vector(efsr$grid$classif.ce, size = 4)
  expect_list(efsr$grid$importance, any.missing = FALSE, len = 4)
  expect_benchmark_result(efsr$benchmark_result)
})

test_that("stability method works", {
  efsr = ensemble_fselect(
    fselector = fs("rfe", n_features = 2, feature_fraction = 0.8),
    task = tsk("sonar"),
    learners = lrns(c("classif.rpart", "classif.featureless")),
    init_resampling = rsmp("subsampling", repeats = 2),
    inner_resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("none")
  )

  expect_number(efsr$stability(stability_measure = "jaccard"))
})

