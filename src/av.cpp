#include <Rcpp.h>
using namespace Rcpp;

// Approval Voting
// [[Rcpp::export]]
List AV_rcpp(List voters, CharacterVector candidates, NumericVector weights) {
  int n_candidates = candidates.size();
  int n_voters = voters.size();
  NumericVector approval_counts(n_candidates);

  // Compute the approval counts for every candidate
  for (int i = 0; i < n_candidates; i++) {
    double candidate_count = 0.0;
    String candidate = candidates[i];
    for (int j = 0; j < n_voters; j++) {
      // Get the candidates of voter j
      CharacterVector voter_candidates = voters[j];

      // Check if the candidate is in the voter's list
      bool candidate_found = std::find(voter_candidates.begin(), voter_candidates.end(), candidate) != voter_candidates.end();
      if (candidate_found) {
        candidate_count += weights[j];
      }
    }
    approval_counts[i] = candidate_count;
  }

  // Normalized approval counts => selection frequency
  double total = sum(weights);
  NumericVector norm_score = approval_counts / total;

  return List::create(
    _["feature"] = candidates,
    _["score"] = approval_counts,
    _["norm_score"] = norm_score
  );
}
