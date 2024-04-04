test_that("esemble feature selection works", {
  res = ensemble_fselect(
    fselector = fs("random_search"),
    task = tsk("sonar"),
    learners = lrns(c("classif.rpart", "classif.featureless")),
    outer_resampling = rsmp("subsampling", repeats = 2),
    inner_resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10)
  )

  expect_data_table(res, nrows = 2)
})


