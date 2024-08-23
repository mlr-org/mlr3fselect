# some collection of voting methods for feature ranking

# @param voters list of feature vectors (best features, each a subset of "candidates")
# @param candidates vector with ALL features.
# Should be shuffled before so that the same tie-breaking rule is applied to all methods.
# @param weights vector of weights, 1-1 correspondence with voters
# @param committee_size number of top N features
# @return data.table with 4 columns: features (mandatory), score, norm_score, borda_score (mandatory).
# Always features are ordered with decreasing `score` (or decreasing `borda_score` if a method returns only a ranking).

approval_voting = function(voters, candidates, weights) {
  # faster R version in case of equal weights
  if (all(weights == 1)) {
    count_tbl = sort(table(unlist(voters)), decreasing = TRUE)
    features_selected = names(count_tbl)
    features_not_selected = setdiff(candidates, features_selected)
    approval_counts = as.vector(count_tbl)

    res_fs = data.table(
      feature = features_selected,
      score = approval_counts,
      norm_score = approval_counts / length(voters)
    )

    res_fns = data.table(
      feature = features_not_selected,
      score = 0,
      norm_score = 0
    )

    res = rbindlist(list(res_fs, res_fns))
  } else {
    # returns AV scores so needs ordering
    res = as.data.table(AV_rcpp(voters, candidates, weights))
    setorderv(res, cols = "score", order = -1)
  }

  add_borda_score(res)
}

satisfaction_approval_voting = function(voters, candidates, weights) {
  # returns SAV scores so needs ordering
  res = as.data.table(SAV_rcpp(voters, candidates, weights))
  setorderv(res, cols = "score", order = -1)

  add_borda_score(res)
}

seq_proportional_approval_voting = function(voters, candidates, weights, committee_size = NULL) {
  if (is.null(committee_size)) {
    committee_size = length(candidates)
  }

  # returns ranked features from best to worst (up to committee_size)
  res = as.data.table(seq_PAV_rcpp(voters, candidates, weights, committee_size))

  add_borda_score(res, n = length(candidates))
}

seq_phragmen_rule = function(voters, candidates, weights, committee_size = NULL) {
  if (is.null(committee_size)) {
    committee_size = length(candidates)
  }

  # returns ranked features from best to worst (up to committee_size)
  res = as.data.table(seq_Phragmen_rcpp(voters, candidates, weights, committee_size))

  add_borda_score(res, n = length(candidates))
}

# add normalized borda scores, `total_num` is needed in case we select a committee
# with size less than the total number of candidates
add_borda_score = function(dt, n = NULL) {
  if (is.null(n)) n = nrow(dt)
  borda_score = NULL # silence data.table note: "no visible global binding"
  dt[, borda_score := if (nrow(dt) == 1) 1 else (n - .I) / (n - 1)]
}
