#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;

// Sequential Proportional Approval Voting
// [[Rcpp::export]]
List seq_PAV_rcpp(List voters, CharacterVector candidates, NumericVector weights, int committee_size) {
  int n_candidates = std::min<int>(candidates.size(), committee_size);
  int n_voters = voters.size();
  CharacterVector committee; // starts empty
  CharacterVector remaining_candidates = clone(candidates); // starts with all candidates

  // transform voters approvals in unsorted sets for faster membership lookups
  std::vector<std::unordered_set<std::string>> voters_sets;
  // Transform each vector into an unordered set
  for (const auto& vec : voters) {
    CharacterVector voter_candidates = as<CharacterVector>(vec);
    std::unordered_set<std::string> candidates_set;
    for (int i = 0; i < voter_candidates.size(); ++i) {
      candidates_set.insert(std::string(voter_candidates[i]));
    }
    voters_sets.push_back(candidates_set);
  }

  // pre-compute harmonic numbers
  // We need from 0 up to all candidates elected in a committee
  NumericVector harmonic(n_candidates + 1, 0.0);
  for (int i = 1; i <= n_candidates; ++i) {
    harmonic[i] = harmonic[i - 1] + 1.0 / i;
  }

  // maintain for every voter the number of candidates that have been elected already from their approval list
  IntegerVector n_elected_approved(voters.size(), 0);

  for (int i = 0; i < n_candidates; ++i) {
    int n_remaining_candidates = remaining_candidates.size();
    int index = 0; // if 1 remaining candidate, this is the index of the best candidate
    String best_candidate;
    // Rcout << "\nRemaining candidates: " << remaining_candidates << "\n";

    // find the 'index' of the best candidate
    if (n_remaining_candidates > 1) {
      double best_score = 0;

      // Compute the marginal contribution to the PAV score that each candidate
      // would have (`marginal_score`) and maintain the largest one (`best_score`)
      for (int j = 0; j < remaining_candidates.size(); ++j) {
        String candidate = remaining_candidates[j];
        double marginal_score = 0;
        //Rcout << "Candidate: " << candidate.get_cstring() << "\n";
        for (int k = 0; k < n_voters; ++k) {
          if (voters_sets[k].find(candidate) != voters_sets[k].end()) {
            int n_elected = n_elected_approved[k];
            //Rcout << "Found candidate in voter set " << k << ", with " << n_elected << " approved candidates\n";
            marginal_score += weights[k] * (harmonic[n_elected + 1] - harmonic[n_elected]);
          }
        }
        //Rcout << "marginal score: " << marginal_score << "\n";
        if (marginal_score > best_score) {
          best_score = marginal_score;
          index = j;
        }
      }
    }

    // Get candidate with max marginal pav score
    best_candidate = remaining_candidates[index];
    // Add best candidate to the committee
    committee.push_back(best_candidate);
    // Remove best candidate from the remaining candidates
    remaining_candidates.erase(index);

    // update the satisfaction of the voters
    for (int k = 0; k < n_voters; ++k) {
      if (voters_sets[k].find(best_candidate) != voters_sets[k].end()) {
        ++n_elected_approved[k];
      }
    }
  }

  return List::create(
    _["feature"] = committee
  );
}
