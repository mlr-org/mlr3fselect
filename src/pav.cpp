#include <Rcpp.h>
#include <random>
#include <unordered_set>
using namespace Rcpp;

// Voter satisfaction => number of (voted) candidates that are in the given committee
IntegerVector voter_satisfaction(const List &voters, const CharacterVector &committee) {
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
double PAV_score(const List &voters, const NumericVector &weights, const CharacterVector &committee, const NumericVector &harmonic) {
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
List seq_PAV_rcpp(List voters, CharacterVector candidates, NumericVector weights, int committee_size) {
  int n_candidates = std::min<int>(candidates.size(), committee_size);
  CharacterVector committee; // starts empty
  CharacterVector remaining_candidates = clone(candidates); // starts with all candidates

  // In order to enforce random tie breaking,
  // shuffle the remaining_candidates so that their order is random
  std::random_device rd; // Obtain a random number from hardware
  std::mt19937 g(rd()); // Seed the generator
  std::shuffle(remaining_candidates.begin(), remaining_candidates.end(), g);

  // pre-compute harmonic numbers
  // We need from 0 up to all candidates elected in a committee
  NumericVector harmonic(n_candidates + 1, 0.0);
  for (int i = 1; i <= n_candidates; ++i) {
    harmonic[i] = harmonic[i - 1] + 1.0 / i;
  }

  for (int i = 0; i < n_candidates; ++i) {
    int n_remaining_candidates = remaining_candidates.size();
    int index = 0; // if 1 remaining candidate, this is the index of the best candidate
    String best_candidate;

    if (n_remaining_candidates > 1) {
      double best_score = 0;
      double pav_score;

      // Add each time a different candidate in the committee and compute the PAV score
      for (int j = 0; j < remaining_candidates.size(); ++j) {
        CharacterVector temp_committee = clone(committee);
        temp_committee.push_back(remaining_candidates[j]);
        pav_score = PAV_score(voters, weights, temp_committee, harmonic);

        // keep track of the maximum PAV score
        if (pav_score > best_score) {
          best_score = pav_score;
          index = j;
        }
      }
    }

    // Get candidate with max pav score
    best_candidate = remaining_candidates[index];
    // Add best candidate to the committee
    committee.push_back(best_candidate);
    // Remove best candidate from the remaining candidates
    remaining_candidates.erase(index);
  }

  return List::create(
    _["feature"] = committee
  );
}

// Sequential Proportional Approval Voting Optimized
// [[Rcpp::export]]
List seq_PAV_rcpp2(List voters, CharacterVector candidates, NumericVector weights, int committee_size) {
  int n_candidates = std::min<int>(candidates.size(), committee_size);
  int n_voters = voters.size();
  CharacterVector committee; // starts empty
  CharacterVector remaining_candidates = clone(candidates); // starts with all candidates

  // In order to enforce random tie breaking,
  // shuffle the remaining_candidates so that their order is random
  std::random_device rd; // Obtain a random number from hardware
  std::mt19937 g(rd()); // Seed the generator
  std::shuffle(remaining_candidates.begin(), remaining_candidates.end(), g);

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
