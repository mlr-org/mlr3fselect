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
# "V3" candidate in all sets, "V1" in half, "V2", "V4" once, "V5" nowhere!
vot2 = list(
  c("V3", "V1", "V2"),
  c("V3", "V1"),
  c("V3", "V4"),
  c("V3")
)
# edge case: all voters voted the same candidates, "V4" and "V5" are nowhere!
vot3 = list(
  c("V3", "V1", "V2"),
  c("V3", "V1", "V2"),
  c("V3", "V1", "V2"),
  c("V3", "V1", "V2")
)
w2 = runif(length(vot2))
w2_equal = rep(1, length(vot2))

test_that("approval voting", {
  # large data
  av = approval_voting(vot, cand, w)
  expect_data_table(av, nrows = length(cand), ncols = 4)
  expect_setequal(colnames(av), c("feature", "score", "norm_score", "borda_score"))
  expect_setequal(av$feature, cand) # all features are there
  expect_true(all(av$score >= 0)) # positive scores
  expect_true(all(av$norm_score >= 0 & av$norm_score <= 1)) # behave like probs
  expect_equal(av$borda_score[1], 1)
  expect_equal(av$borda_score[length(cand)], 0)

  av_equalw = approval_voting(vot, cand, w_equal)
  expect_data_table(av_equalw, nrows = length(cand), ncols = 4)
  expect_true(all(av_equalw$score >= 0)) # positive scores
  expect_true(all(av_equalw$norm_score >= 0 & av_equalw$norm_score <= 1)) # behave like probs
  expect_equal(av_equalw$borda_score[1], 1)
  expect_equal(av_equalw$borda_score[length(cand)], 0)
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

  # tie breaking
  av3 = approval_voting(vot3, cand2, w2)
  expect_contains(av3$feature[1:3], c("V1", "V2", "V3"))
  expect_true(length(unique(av3$score[1:3])) == 1) # all scores the same
  expect_contains(av3$feature[4:5], c("V4", "V5"))
})

test_that("satisfaction approval voting", {
  # large data
  sav = satisfaction_approval_voting(vot, cand, w)
  expect_data_table(sav, nrows = length(cand), ncols = 4)
  expect_setequal(colnames(sav), c("feature", "score", "norm_score", "borda_score"))
  expect_setequal(sav$feature, cand) # all features are there
  expect_true(all(sav$score >= 0)) # positive scores
  expect_true(all(sav$norm_score >= 0 & sav$norm_score <= 1)) # behave like probs
  expect_equal(sav$borda_score[1], 1)
  expect_equal(sav$borda_score[length(cand)], 0)

  sav_equalw = satisfaction_approval_voting(vot, cand, w_equal)
  expect_data_table(sav_equalw, nrows = length(cand), ncols = 4)
  expect_true(all(sav_equalw$score >= 0)) # positive scores
  expect_true(all(sav_equalw$norm_score >= 0 & sav_equalw$norm_score <= 1)) # behave like probs
  expect_equal(sav_equalw$borda_score[1], 1)
  expect_equal(sav_equalw$borda_score[length(cand)], 0)
  # using unequal weights, feature rankings should be different
  expect_false(identical(sav$feature, sav_equalw$feature))

  # small data
  sav2 = satisfaction_approval_voting(vot2, cand2, w2)
  expect_equal(sav2$feature[1:2], c("V3", "V1"))
  expect_equal(sav2$feature[5], "V5")
  expect_equal(sav2[feature == "V3", norm_score], 1) # always present
  expect_equal(sav2[feature == "V5", norm_score], 0) # never present

  sav2_equalw = satisfaction_approval_voting(vot2, cand2, w2_equal)
  expect_equal(sav2_equalw$feature, c("V3", "V1", "V4", "V2", "V5"))

  # tie breaking
  sav3 = satisfaction_approval_voting(vot3, cand2, w2)
  expect_contains(sav3$feature[1:3], c("V1", "V2", "V3"))
  expect_true(length(unique(sav3$score[1:3])) == 1) # all scores the same
  expect_contains(sav3$feature[4:5], c("V4", "V5"))
})

test_that("sequential proportional approval voting", {
  # large data
  size = 5 # get top 5 ranked features
  sp = seq_proportional_approval_voting(vot, cand, w, size)
  expect_data_table(sp, nrows = size, ncols = 2)
  expect_setequal(colnames(sp), c("feature", "borda_score"))
  sp1 = seq_proportional_approval_voting(vot, cand, w, committee_size = 3)
  expect_equal(sp$feature[1:3], sp1$feature[1:3]) # house monotonicity

  spe = seq_proportional_approval_voting(vot, cand, w_equal, size)
  expect_data_table(spe, nrows = size, ncols = 2)
  # using unequal weights, feature rankings should be different
  expect_false(identical(spe$feature, sp$feature))

  # small data
  seq_pav2 = seq_proportional_approval_voting(vot2, cand2, w2)
  expect_data_table(seq_pav2, nrows = length(cand2), ncols = 2)
  expect_setequal(colnames(seq_pav2), c("feature", "borda_score"))
  expect_equal(seq_pav2$borda_score[1], 1)
  expect_equal(seq_pav2$borda_score[length(cand2)], 0)

  seq_pav2_equal = seq_proportional_approval_voting(vot2, cand2, w2_equal)
  expect_data_table(seq_pav2_equal, nrows = length(cand2), ncols = 2)
  expect_setequal(colnames(seq_pav2_equal), c("feature", "borda_score"))
  expect_equal(seq_pav2_equal$borda_score[1], 1)
  expect_equal(seq_pav2_equal$borda_score[length(cand2)], 0)

  # tie breaking
  seq_pav3 = seq_proportional_approval_voting(vot3, cand2, w2)
  expect_contains(seq_pav3$feature[1:3], c("V1", "V2", "V3"))
  expect_contains(seq_pav3$feature[4:5], c("V4", "V5"))
})
