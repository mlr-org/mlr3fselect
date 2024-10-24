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

  // Build a vector of voter indexes for each candidate: which voters approved a candidate
  std::vector<std::vector<int>> approving_voters(candidates.size());

  // For every candidate
  for (int i = 0; i < candidates.size(); ++i) {
    String candidate = candidates[i];
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
    int index = 0; // index of 'remaining_candidates' with min load
    double min_load = std::numeric_limits<double>::max(); // +Inf
    String best_candidate;
    //Rcout << "\nRemaining candidates: " << remaining_candidates << "\n";

    // Compute the new loads for approving voters if we are adding a candidate
    // and maintain the minimum one
    for (int j = 0; j < n_remaining_candidates; ++j) {
      double total_weight = 0;
      double sum_current_loads = 0;
      String candidate = remaining_candidates[j];

      // need original candidate index for accessing 'approving_voters'
      int candidate_index = get_index(candidates, candidate);
      for (int v : approving_voters[candidate_index]) {
        sum_current_loads += weights[v] * current_loads[v];
        total_weight += weights[v];
      }
      double new_load = (1.0 + sum_current_loads) / total_weight;
      //Rcout << "New load: " << new_load << ", min prev index: " << index << " Min: " << min_load << "\n";
      if (new_load < min_load) {
        min_load = new_load;
        index = j;
      }
    }

    // Get candidate with min load
    best_candidate = remaining_candidates[index];
    //Rcout << "Best candidate: " << best_candidate.get_cstring() << "\n";

    // Add best candidate to the committee
    committee.push_back(best_candidate);
    // Remove best candidate from the remaining candidates
    remaining_candidates.erase(index);
    // need original candidate index for accessing 'approving_voters'
    int best_candidate_index = get_index(candidates, best_candidate);
    // Update current loads
    for (int v : approving_voters[best_candidate_index]) {
      current_loads[v] = min_load;
    }
  }

  return List::create(
    _["feature"] = committee
  );
}
