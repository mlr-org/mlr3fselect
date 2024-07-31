#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_approval_voting(List voters, CharacterVector candidates, NumericVector weights) {
  int n_candidates = candidates.size();
  int n_voters = voters.size();
  NumericVector approval_counts(n_candidates);

  // Create a candidate-voter matrix and compute the approval counts
  for (int i = 0; i < n_candidates; i++) {
    double candidate_count = 0.0;
    for (int j = 0; j < n_voters; j++) {
      // Get the candidates of voter j
      CharacterVector cv = voters[j];
      // Check if the candidate is in the voter's list
      if (std::find(cv.begin(), cv.end(), candidates[i]) != cv.end()) {
        candidate_count += weights[j];
      }
    }
    approval_counts[i] = candidate_count;
  }

  // Normalize approval counts
  double total_weights = sum(weights);
  for (int i = 0; i < n_candidates; i++) {
    approval_counts[i] /= total_weights;
  }

  return List::create(
    _["feature"] = candidates,
    _["score"] = approval_counts
  );
}
