test_that("fselect_nested function works", {
  rr = fselect_nested(method = "random_search", task = tsk("pima"), learner = lrn("classif.rpart"), 
    inner_resampling = rsmp ("holdout"), outer_resampling = rsmp("cv", folds = 3), measure = msr("classif.ce"), 
    term_evals = 2)

  expect_resample_result(rr)
  expect_equal(rr$resampling$id, "cv")
  expect_equal(rr$resampling$iters, 3)
  expect_data_table(extract_inner_fselect_results(rr), nrows = 3)
  expect_class(rr$learners[[1]], "AutoFSelector")
  expect_equal(rr$learners[[1]]$fselect_instance$objective$resampling$id, "holdout")
})
