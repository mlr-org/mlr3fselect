#include <Rcpp.h>
#include <unordered_set>
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

// Voter satisfaction => number of (voted) candidates that are in the given committee
IntegerVector voter_satisfaction(List voters, CharacterVector committee) {
  int n_voters = voters.size();
  IntegerVector voter_sat(n_voters, 0);

  // Convert the committee to an `unordered_set` for faster lookups (O(1) on average)
  std::unordered_set<std::string> committee_set;
  for (int i = 0; i < committee.size(); ++i) {
    committee_set.insert(std::string(committee[i]));
  }

  for (int j = 0; j < n_voters; ++j) {
    CharacterVector voter_candidates = voters[j];

    for (int i = 0; i < voter_candidates.size(); ++i) {
      if (committee_set.find(std::string(voter_candidates[i])) != committee_set.end()) {
        voter_sat[j] += 1;
      }
    }
  }
  return voter_sat;
}

// PAV score is the weighted sum of the harmonic numbers of the number
// of elected candidates for each voter (the higher, the better).
double PAV_score(List voters, NumericVector weights, CharacterVector committee, const NumericVector &harmonic) {
  // voter's satisfaction => number of candidates that got elected in the committee
  IntegerVector n_candidates_elected = voter_satisfaction(voters, committee);

  double score = 0.0;
  for (int j = 0; j < voters.size(); ++j) {
    // 0 elected candidates => score = 0
    // 1 elected candidates => score = weight * 1
    // 2 elected candidates => score = weight * (1 + 1/2)
    score += weights[j] * harmonic[n_candidates_elected[j]];
  }

  return score;
}

// Sequential Proportional Approval Voting
// [[Rcpp::export]]
List seq_PAV_rcpp(List voters, CharacterVector candidates, NumericVector weights) {
  int n_candidates = candidates.size();
  CharacterVector committee; // starts empty
  CharacterVector remaining_candidates = clone(candidates); // starts with all candidates

  // pre-compute harmonic numbers
  // We need from 0 up to all candidates elected in a committee
  NumericVector harmonic(n_candidates + 1, 0.0);
  for (int i = 1; i <= n_candidates; ++i) {
    harmonic[i] = harmonic[i - 1] + 1.0 / i;
  }

  for (int i = 0; i < n_candidates; ++i) {
    int n_remaining_candidates = remaining_candidates.size();
    int index;
    String best_candidate;
    // Rcout << n_remaining_candidates << std::endl;

    if (n_remaining_candidates == 1) {
      // Only one candidate remaining so that's our best candidate!
      index = 0;
      best_candidate = remaining_candidates[index];
    } else {
      NumericVector pav_scores(n_remaining_candidates);

      // Add each time a different candidate in the committee and compute the PAV score
      for (int j = 0; j < remaining_candidates.size(); ++j) {
        CharacterVector temp_committee = clone(committee);
        temp_committee.push_back(remaining_candidates[j]);
        pav_scores[j] = PAV_score(voters, weights, temp_committee, harmonic);
      }

      // Find the max PAV score
      double max_pav_score = max(pav_scores);

      // Find candidate with max PAV score
      std::vector<int> max_indices;
      for (int j = 0; j < pav_scores.size(); ++j) {
        if (pav_scores[j] == max_pav_score) {
          max_indices.push_back(j);
        }
      }

      // Get random index from those that have max pav score
      index = max_indices[std::floor(R::runif(0, max_indices.size()))];
      best_candidate = remaining_candidates[index];
    }

    // Add best candidate to the committee
    committee.push_back(best_candidate);
    // Remove best candidate from the remaining candidates
    remaining_candidates.erase(index);
  }

  return List::create(
    _["feature"] = committee
  );
}

// Reverse Sequential Proportional Approval Voting
// [[Rcpp::export]]
List revseq_PAV_rcpp(List voters, CharacterVector candidates, NumericVector weights) {
  int n_candidates = candidates.size();
  CharacterVector committee; // starts empty
  CharacterVector remaining_candidates = clone(candidates); // starts with all candidates

  // pre-compute harmonic numbers
  // We need from 0 up to all candidates elected in a committee
  NumericVector harmonic(n_candidates + 1, 0.0);
  for (int i = 1; i <= n_candidates; ++i) {
    harmonic[i] = harmonic[i - 1] + 1.0 / i;
  }

  for (int i = 0; i < n_candidates; ++i) {
    int n_remaining_candidates = remaining_candidates.size();
    int index;
    String worst_candidate;
    // Rcout << n_remaining_candidates << std::endl;

    if (n_remaining_candidates == 1) {
      // Only one candidate remaining so that's the last (worst) candidate!
      index = 0;
      worst_candidate = remaining_candidates[index];
    } else {
      NumericVector pav_scores(n_remaining_candidates);

      // Remove each time a different candidate from the committee and compute the PAV score
      for (int j = 0; j < n_remaining_candidates; ++j) {
        CharacterVector temp_committee = clone(remaining_candidates);
        temp_committee.erase(j);
        pav_scores[j] = PAV_score(voters, weights, temp_committee, harmonic);
      }

      // Find the max PAV score
      double max_value = max(pav_scores);

      // Find candidate whose removal resulted in the committee with max PAV score,
      // i.e. the one that decreases the committee's score the least
      std::vector<int> max_indices;
      for (int j = 0; j < pav_scores.size(); ++j) {
        if (pav_scores[j] == max_value) {
          max_indices.push_back(j);
        }
      }

      // Get random index from those that have max pav score
      index = max_indices[std::floor(R::runif(0, max_indices.size()))];
      worst_candidate = remaining_candidates[index];
    }

    // Add worst candidate to the committee
    committee.push_back(worst_candidate);
    // Remove worst candidate from the remaining candidates
    remaining_candidates.erase(index);
  }

  // Reverse the committee candidates (so that order is first:best => last:worst)
  std::reverse(committee.begin(), committee.end());

  return List::create(
    _["feature"] = committee
  );
}
