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

  expect_data_table(efsr$result, nrows = 4)
  expect_list(efsr$result$features, any.missing = FALSE, len = 4)
  expect_vector(efsr$result$n_features, size = 4)
  expect_vector(efsr$result$classif.ce, size = 4)
  expect_list(efsr$result$importance, any.missing = FALSE, len = 4)
  expect_benchmark_result(efsr$benchmark_result)

  expect_number(efsr$stability(stability_measure = "jaccard"))
  feature_ranking = efsr$feature_ranking()
  expect_data_table(feature_ranking, nrows = 60)
  expect_names(names(feature_ranking), identical.to = c("feature", "inclusion_probability"))
})
