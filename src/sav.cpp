#include <Rcpp.h>
using namespace Rcpp;

// Satisfaction Approval Voting
// [[Rcpp::export]]
List SAV_rcpp(List voters, CharacterVector candidates, NumericVector weights) {
  int n_candidates = candidates.size();
  int n_voters = voters.size();
  NumericVector satisfaction_scores(n_candidates);
  NumericVector voter_candidate_counts(n_voters);
  double total = 0;

  // Precompute #candidates per voter and total normalization factor
  for (int j = 0; j < n_voters; ++j) {
    voter_candidate_counts[j] = as<CharacterVector>(voters[j]).size();
    total += weights[j] / voter_candidate_counts[j];
  }

  // Compute the satisfaction scores for every candidate
  for (int i = 0; i < n_candidates; ++i) {
    String candidate = candidates[i];
    for (int j = 0; j < n_voters; ++j) {
      // Get the candidates of voter j
      CharacterVector voter_candidates = voters[j];
      // Check if the candidate is in the voter's list
      bool candidate_found = std::find(voter_candidates.begin(), voter_candidates.end(), candidate) != voter_candidates.end();
      if (candidate_found) {
        satisfaction_scores[i] += weights[j] / voter_candidate_counts[j];
      }
    }
  }

  // Compute normalized satisfaction score
  NumericVector norm_score = satisfaction_scores / total;

  return List::create(
    _["feature"] = candidates,
    _["score"] = satisfaction_scores,
    _["norm_score"] = norm_score
  );
}
