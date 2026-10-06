// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <algorithm>
#include <climits>
#include <cstdint>
#include <string>
#include <unordered_set>
#include <vector>
#include "imac.h"

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// Every inequality matrix is identified by its strict upper triangle, read
// row by row, where each cell is one of -1, 0, 1. The triangle is stored as a
// key: a base-3 number when it has at most 40 cells (3^40 < 2^64), otherwise
// a string with one character per cell.

inline void key_push(uint64_t& key, int cell) { key = key * 3 + (cell + 1); }
inline void key_push(std::string& key, int cell) {
  key.push_back(static_cast<char>(cell + 1));
}
inline void key_clear(uint64_t& key) { key = 0; }
inline void key_clear(std::string& key) { key.clear(); }

// fill cells (in the order they were pushed) back from a key
inline void key_decode(uint64_t key, std::vector<int>& cells) {
  for (std::size_t c = cells.size(); c-- > 0; ) {
    cells[c] = static_cast<int>(key % 3) - 1;
    key /= 3;
  }
}
inline void key_decode(const std::string& key, std::vector<int>& cells) {
  for (std::size_t c = 0; c < cells.size(); c++) cells[c] = key[c] - 1;
}

// Collects the unique inequality matrices produced by permutations
// [begin, end). Permutation r assigns dependent values to the stimuli by
// writing r in base m (m = number of values), with the last stimulus as the
// least significant digit.
template <typename Key>
struct Collector {
  const std::vector<int>& classes;  // m x m table of imac_classify results
  std::size_t m, k;
  std::unordered_set<Key> found;

  Collector(const std::vector<int>& classes, std::size_t m, std::size_t k)
    : classes(classes), m(m), k(k) {}

  void collect(std::size_t begin, std::size_t end) {
    if (begin >= end) return;
    // digits of the first permutation in this range
    std::vector<std::size_t> digits(k);
    std::size_t rest = begin;
    for (std::size_t j = k; j-- > 0; ) {
      digits[j] = rest % m;
      rest /= m;
    }
    Key key;
    for (std::size_t r = begin; r < end; r++) {
      key_clear(key);
      for (std::size_t i = 0; i < k; i++) {
        const int* row = &classes[digits[i] * m];
        for (std::size_t j = i + 1; j < k; j++) key_push(key, row[digits[j]]);
      }
      found.insert(key);
      // advance to the next permutation
      for (std::size_t j = k; j-- > 0; ) {
        if (++digits[j] < m) break;
        digits[j] = 0;
      }
    }
  }
};

// m^k, or 0 if it does not fit in a std::size_t
std::size_t count_permutations(std::size_t m, std::size_t k) {
  std::size_t total = 1;
  for (std::size_t j = 0; j < k; j++) {
    if (total > SIZE_MAX / m) return 0;
    total *= m;
  }
  return total;
}

template <typename Key>
cube unique_matrices(const std::vector<int>& classes, std::size_t m,
                     std::size_t k, std::size_t total) {
  Collector<Key> collector(classes, m, k);
  collector.collect(0, total);

  // sort, so that the output does not depend on the order of discovery
  std::vector<Key> keys(collector.found.begin(), collector.found.end());
  std::sort(keys.begin(), keys.end());

  cube ordinals(k, k, keys.size());
  ordinals.fill(2);
  std::vector<int> cells(k * (k - 1) / 2);
  for (uword s = 0; s < keys.size(); s++) {
    key_decode(keys[s], cells);
    std::size_t c = 0;
    for (uword i = 0; i < k; i++) {
      for (uword j = i + 1; j < k; j++) ordinals(i, j, s) = cells[c++];
    }
  }
  return ordinals;
}

// [[Rcpp::export]]
List brutes(NumericVector dependent, int length, arma::vec thresholds,
            bool return_permutations = false) {
  if (dependent.size() == 0) stop("'dependent' must not be empty.");
  if (length < 1) stop("'length' must be at least 1.");
  imac_check_thresholds(thresholds);

  const std::size_t m = dependent.size();
  const std::size_t k = length;
  const std::size_t total = count_permutations(m, k);
  if (total == 0) {
    stop("There are too many permutations (length(dependent)^length) to "
         "enumerate.");
  }

  // classify every pair of dependent values once
  std::vector<int> classes(m * m);
  for (std::size_t a = 0; a < m; a++) {
    for (std::size_t b = 0; b < m; b++) {
      classes[a * m + b] = imac_classify(dependent[a] - dependent[b],
                                         thresholds[0], thresholds[1]);
    }
  }

  cube ordinals;
  if (k * (k - 1) / 2 <= 40) {
    ordinals = unique_matrices<uint64_t>(classes, m, k, total);
  } else {
    ordinals = unique_matrices<std::string>(classes, m, k, total);
  }

  RObject permutations = R_NilValue;
  if (return_permutations) {
    if (total > static_cast<std::size_t>(INT_MAX)) {
      stop("There are too many permutations to return; use "
           "'return_permutations = FALSE'.");
    }
    NumericMatrix out(static_cast<int>(total), static_cast<int>(k));
    for (std::size_t r = 0; r < total; r++) {
      std::size_t rest = r;
      for (std::size_t j = k; j-- > 0; ) {
        out(r, j) = dependent[rest % m];
        rest /= m;
      }
    }
    permutations = out;
  }

  return List::create(
    Named("permutations") = permutations,
    Named("inequality_matrices") = ordinals,
    Named("n") = static_cast<double>(ordinals.n_slices));
}
