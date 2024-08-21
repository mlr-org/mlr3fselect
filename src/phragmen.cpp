#include <Rcpp.h>
#include <random>
using namespace Rcpp;

// helper function to get the index of element `s` from vector `v, O(n) complexity
int get_index(const CharacterVector &v, const String &s) {
  int index = -1; // not found
  auto it = std::find(v.begin(), v.end(), s);
  // If `s` was found return the index
  if (it != v.end()) {
    index = it - v.begin();
  }
  return(index);
}

// Sequential Phragmen's Rule
// [[Rcpp::export]]
List seq_Phragmen_rcpp(List voters, CharacterVector candidates, NumericVector weights, int committee_size) {
  int n_candidates = std::min<int>(candidates.size(), committee_size);
  CharacterVector committee; // starts empty
  CharacterVector remaining_candidates = clone(candidates); // starts with all candidates

  // Build a vector of indexes corresponding to the voters that approved each candidate
  std::vector<std::vector<int>> approving_voters(candidates.size());

  // For every candidate
  for (int i = 0; i < candidates.size(); ++i) {
    String candidate = candidates[i];
    // Get each voter's candidates
    for (int j = 0; j < voters.size(); ++j) {
      // Get the candidates of voter j
      CharacterVector voter_candidates = voters[j];

      // Check if the candidate is in the voter's list
      bool candidate_found = std::find(voter_candidates.begin(), voter_candidates.end(), candidate) != voter_candidates.end();
      if (candidate_found) {
        approving_voters[i].push_back(j); // add voter index
      }
    }
  }

  // initialize loads
  NumericVector current_loads(voters.size(), 0.0);

  // build committee sequentially, stop at `n_candidates`
  for (int i = 0; i < n_candidates; ++i) {
    int n_remaining_candidates = remaining_candidates.size();
    int index;
    String best_candidate;
    std::vector<double> new_loads(n_remaining_candidates, std::numeric_limits<double>::max());

    // Compute the loads for approving voters if we are adding a candidate
    for (int j = 0; j < n_remaining_candidates; ++j) {
      double total_weight = 0.0;
      double sum_current_loads = 0.0;
      String candidate = remaining_candidates[j];
      int candidate_index = get_index(candidates, candidate);
      Rcout << candidate.get_cstring() << " " << candidate_index << std::endl;

      for (int v : approving_voters[candidate_index]) {
        sum_current_loads += current_loads[v];
        total_weight += weights[v];
      }
      new_loads[i] = (1.0 + sum_current_loads) / total_weight;
    }
    Rcout << "new_loads: ";
    for (const auto& load : new_loads) {
      Rcout << load << " ";
    }
    Rcout << std::endl;


    // Find the minimum load
    double min_load = *std::min_element(new_loads.begin(), new_loads.end());

    // Find candidate with minimum load
    std::vector<int> min_indices;
    for (int j = 0; j < new_loads.size(); ++j) {
      if (new_loads[j] == min_load) {
        min_indices.push_back(j);
      }
    }

    // Get random index from those that have min load
    index = min_indices[std::floor(R::runif(0, min_indices.size()))];
    best_candidate = remaining_candidates[index];

    // Add best candidate to the committee
    committee.push_back(best_candidate);
    // Remove best candidate from the remaining candidates
    remaining_candidates.erase(index);
    // Update current loads
    int best_candidate_index = get_index(candidates, best_candidate);
    for (int v : approving_voters[best_candidate_index]) {
      current_loads[v] = new_loads[index];
    }
  }

  return List::create(
    _["feature"] = committee
  );
}
