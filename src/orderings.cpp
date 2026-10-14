// Copyright 2026 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <numeric>
#include <string>
#include <vector>

#include "imac.h"
#include "pattern_key.h"

namespace {

// Adds the pattern of items with the given ranks: item i is larger than item
// j if its rank is higher, and approximately equal if the ranks are equal.
void AddRanks(const std::vector<int>& ranks, KeySet<uint64_t>* patterns) {
  uint64_t key = 0;
  for (std::size_t i = 0; i < ranks.size(); i++) {
    for (std::size_t j = i + 1; j < ranks.size(); j++) {
      KeyPush(&key, (ranks[i] > ranks[j]) - (ranks[i] < ranks[j]));
    }
  }
  patterns->Add(key);
}

// Adds every weak ordering of k items: every split of the items into groups,
// written as a restricted growth string (group[i] is at most one more than
// the largest earlier group), with every order of the groups.
void AddWeakOrderings(std::size_t k, KeySet<uint64_t>* patterns) {
  std::vector<int> group(k, 0);
  std::vector<int> ranks(k);
  while (true) {
    const int groups = *std::max_element(group.begin(), group.end()) + 1;
    std::vector<int> order(groups);
    std::iota(order.begin(), order.end(), 0);
    do {
      for (std::size_t i = 0; i < k; i++) ranks[i] = order[group[i]];
      AddRanks(ranks, patterns);
    } while (std::next_permutation(order.begin(), order.end()));
    Rcpp::checkUserInterrupt();

    // next restricted growth string
    std::size_t i = k;
    while (i-- > 1) {
      const int largest = *std::max_element(group.begin(), group.begin() + i);
      if (group[i] <= largest) {
        group[i]++;
        std::fill(group.begin() + i + 1, group.end(), 0);
        break;
      }
    }
    if (i == 0) return;
  }
}

// Adds every semiorder of k items. Sorting the items by value, the items
// approximately equal to the item at position p and above it are those at
// positions p + 1, ..., reach[p], where reach is non-decreasing and
// reach[p] >= p. There are Catalan(k) such sequences, one for each
// unlabelled semiorder; each is labelled in every way, and patterns that
// are labelled alike are kept once.
void AddSemiorders(std::size_t k, KeySet<uint64_t>* patterns) {
  std::vector<std::size_t> reach(k);
  for (std::size_t p = 0; p < k; p++) reach[p] = p;
  std::vector<std::size_t> position(k);
  while (true) {
    std::vector<std::size_t> item(k);  // item[p] is the item at position p
    std::iota(item.begin(), item.end(), 0);
    do {
      for (std::size_t p = 0; p < k; p++) position[item[p]] = p;
      uint64_t key = 0;
      for (std::size_t a = 0; a < k; a++) {
        for (std::size_t b = a + 1; b < k; b++) {
          const std::size_t pa = position[a];
          const std::size_t pb = position[b];
          int cell;
          if (pa < pb) {
            cell = pb <= reach[pa] ? 0 : -1;
          } else {
            cell = pa <= reach[pb] ? 0 : 1;
          }
          KeyPush(&key, cell);
        }
      }
      patterns->Add(key);
    } while (std::next_permutation(item.begin(), item.end()));
    Rcpp::checkUserInterrupt();

    // next non-decreasing sequence with p <= reach[p] <= k - 1
    std::size_t p = k;
    while (p-- > 0) {
      if (reach[p] < k - 1) {
        reach[p]++;
        for (std::size_t q = p + 1; q < k; q++) {
          reach[q] = std::max(q, reach[p]);
        }
        break;
      }
    }
    if (p == static_cast<std::size_t>(-1)) return;
  }
}

}  // namespace

// [[Rcpp::export(name = ".orderings")]]
Rcpp::List Orderings(int length, const std::string& ordering) {
  const bool weak = IsWeakOrdering(ordering);
  const std::size_t k = length;
  if (CountCells(k) > kMaxPackedCells) {
    Rcpp::stop("There are too many patterns to enumerate.");
  }
  KeySet<uint64_t> patterns;
  if (weak) {
    AddWeakOrderings(k, &patterns);
  } else {
    AddSemiorders(k, &patterns);
  }
  const std::vector<uint64_t> keys = patterns.TakeSorted();
  return Rcpp::List::create(
      Rcpp::Named("inequality_matrices") = KeysToCube(keys, k),
      Rcpp::Named("n") = static_cast<double>(keys.size()));
}
