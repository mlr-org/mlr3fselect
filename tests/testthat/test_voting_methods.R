# large data example
set.seed(42)
n_candidates = 800 # n_features
n_voters = 200 # n_models

cand = paste0("V", seq_len(n_candidates))
vot = lapply(1:n_voters, function(x) sample(cand, size = sample(2:30, 1)))
w = runif(n_voters)
w_equal = rep(1, n_voters)

# small data example
cand2 = paste0("V", seq_len(5))
vot2 = list(
  # "V3" candidate in all sets, "V1" in half, "V2", "V4" once, "V5" nowhere!
  c("V3", "V1", "V2"),
  c("V3", "V1"),
  c("V3", "V4"),
  c("V3"))
w2 = runif(length(vot2))
w2_equal = rep(1, length(vot2))

test_that("approval voting", {
  # large data
  av = approval_voting(vot, cand, w)
  expect_data_table(av, nrows = length(cand), ncols = 3)
  expect_setequal(colnames(av), c("feature", "score", "norm_score"))
  expect_setequal(av$feature, cand) # all features are there
  expect_true(all(av$score >= 0)) # positive scores
  expect_true(all(av$norm_score >= 0 & av$norm_score <= 1)) # behave like probs

  av_equalw = approval_voting(vot, cand, w_equal)
  expect_data_table(av_equalw, nrows = length(cand), ncols = 3)
  expect_true(all(av_equalw$score >= 0)) # positive scores
  expect_true(all(av_equalw$norm_score >= 0 & av_equalw$norm_score <= 1)) # behave like probs
  # using unequal weights, feature rankings should be different
  expect_false(identical(av$feature, av_equalw$feature))

  # small data
  av2 = approval_voting(vot2, cand2, w2)
  expect_equal(av2$feature[1:2], c("V3", "V1"))
  expect_equal(av2$feature[5], "V5")
  expect_equal(av2[feature == "V3", norm_score], 1) # always present
  expect_equal(av2[feature == "V5", norm_score], 0) # never present

  av2_equalw = approval_voting(vot2, cand2, w2_equal)
  expect_equal(av2_equalw$feature[1:2], c("V3", "V1"))
  expect_equal(av2_equalw$feature[5], "V5")
  expect_equal(av2_equalw$score, c(4, 2, 1, 1, 0))
  expect_equal(av2_equalw$norm_score, c(1, 0.5, 0.25, 0.25, 0))
})
