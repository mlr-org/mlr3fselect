# large data example
set.seed(42)
n_candidates = 800 # n_features
n_voters = 200 # best feature sets

cand = paste0("V", seq_len(n_candidates))
vot = lapply(1:n_voters, function(x) sample(cand, size = sample(2:30, 1)))
w = runif(n_voters)
w_equal = rep(1, n_voters)

# small data example
cand2 = paste0("V", seq_len(4))
vot2 = list(
  # "V3" candidate in all sets, "V1" in half, "V2", "V4" once
  c("V3", "V1", "V2"),
  c("V3", "V1"),
  c("V3", "V4"),
  c("V3"))
w2 = runif(4)
w2_equal = rep(1, 4)

test_that("approval voting", {
  # large data
  av = approval_voting(vot, cand, w)
  expect_data_table(av, nrows = length(cand), ncols = 2)
  expect_setequal(colnames(av), c("feature", "score"))
  expect_setequal(av$feature, cand)
  # check that they behave like probabilities
  expect_true(all(av$score >= 0 & av$score <= 1))

  av_equalw = approval_voting(vot, cand, w_equal)
  expect_data_table(av_equalw, nrows = length(cand), ncols = 2)
  # check that they behave like probabilities
  expect_true(all(av_equalw$score >= 0 & av_equalw$score <= 1))
  # using unequal weights, feature rankings should be different
  expect_false(identical(av$feature, av_equalw$feature))

  # small data
  av2 = approval_voting(vot2, cand2, w2)
  expect_equal(av2$feature[1:2], c("V3", "V1"))
  expect_equal(av2$score[1], 1)

  av2_equalw = approval_voting(vot2, cand2, w2_equal)
  expect_equal(av2_equalw$feature[1:2], c("V3", "V1"))
  expect_equal(av2_equalw$score[1], 1)
  # using unequal weights, feature rankings should be different
  expect_false(identical(av2$feature, av2_equalw$feature))
})
