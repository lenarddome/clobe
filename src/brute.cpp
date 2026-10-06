// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include <algorithm>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <string>
#include <unordered_set>
#include <vector>

#include "imac.h"

namespace {

// Every inequality matrix is identified by its strict upper triangle, read
// row by row, where each cell is one of -1, 0, 1. The triangle is stored as a
// key: a base-3 number when it has at most kMaxPackedCells cells
// (3^40 < 2^64), otherwise a string with one character per cell.
constexpr std::size_t kMaxPackedCells = 40;

inline void KeyPush(uint64_t* key, int cell) { *key = *key * 3 + (cell + 1); }
inline void KeyPush(std::string* key, int cell) {
  key->push_back(static_cast<char>(cell + 1));
}
inline void KeyClear(uint64_t* key) { *key = 0; }
inline void KeyClear(std::string* key) { key->clear(); }

// Fills cells, in the order they were pushed, back from a key.
inline void KeyDecode(uint64_t key, std::vector<int>* cells) {
  for (std::size_t c = cells->size(); c-- > 0;) {
    (*cells)[c] = static_cast<int>(key % 3) - 1;
    key /= 3;
  }
}
inline void KeyDecode(const std::string& key, std::vector<int>* cells) {
  for (std::size_t c = 0; c < cells->size(); c++) (*cells)[c] = key[c] - 1;
}

// Collects the unique inequality matrices produced by a range of
// permutations. Permutation r assigns dependent values to the stimuli by
// writing r in base m (m = number of values), with the last stimulus as the
// least significant digit.
template <typename Key>
class Collector {
 public:
  // classes is the m x m table of ImacClassify results for each pair of
  // dependent values; it must outlive the collector.
  Collector(const std::vector<int>& classes, std::size_t m, std::size_t k)
      : classes_(classes), m_(m), k_(k) {}

  // Adds the patterns of permutations [begin, end).
  void Collect(std::size_t begin, std::size_t end) {
    if (begin >= end) return;
    // digits of the first permutation in this range
    std::vector<std::size_t> digits(k_);
    std::size_t rest = begin;
    for (std::size_t j = k_; j-- > 0;) {
      digits[j] = rest % m_;
      rest /= m_;
    }
    Key key;
    for (std::size_t r = begin; r < end; r++) {
      KeyClear(&key);
      for (std::size_t i = 0; i < k_; i++) {
        const int* row = &classes_[digits[i] * m_];
        for (std::size_t j = i + 1; j < k_; j++) KeyPush(&key, row[digits[j]]);
      }
      found_.insert(key);
      // advance to the next permutation
      for (std::size_t j = k_; j-- > 0;) {
        if (++digits[j] < m_) break;
        digits[j] = 0;
      }
    }
  }

  const std::unordered_set<Key>& found() const { return found_; }

 private:
  const std::vector<int>& classes_;
  std::size_t m_;
  std::size_t k_;
  std::unordered_set<Key> found_;
};

// Returns m^k, or 0 if it does not fit in a std::size_t.
std::size_t CountPermutations(std::size_t m, std::size_t k) {
  std::size_t total = 1;
  for (std::size_t j = 0; j < k; j++) {
    if (total > SIZE_MAX / m) return 0;
    total *= m;
  }
  return total;
}

template <typename Key>
arma::cube UniqueMatrices(const std::vector<int>& classes, std::size_t m,
                          std::size_t k, std::size_t total) {
  Collector<Key> collector(classes, m, k);
  collector.Collect(0, total);

  // sort, so that the output does not depend on the order of discovery
  std::vector<Key> keys(collector.found().begin(), collector.found().end());
  std::sort(keys.begin(), keys.end());

  arma::cube ordinals(k, k, keys.size());
  ordinals.fill(2);
  std::vector<int> cells(k * (k - 1) / 2);
  for (arma::uword s = 0; s < keys.size(); s++) {
    KeyDecode(keys[s], &cells);
    std::size_t c = 0;
    for (arma::uword i = 0; i < k; i++) {
      for (arma::uword j = i + 1; j < k; j++) ordinals(i, j, s) = cells[c++];
    }
  }
  return ordinals;
}

}  // namespace

// [[Rcpp::export(name = "brutes")]]
Rcpp::List Brutes(const Rcpp::NumericVector& dependent, int length,
                  const arma::vec& thresholds,
                  const std::string& ordering = "semiorder",
                  bool return_permutations = false) {
  if (dependent.size() == 0) Rcpp::stop("'dependent' must not be empty.");
  if (length < 1) Rcpp::stop("'length' must be at least 1.");
  CheckThresholds(thresholds);
  const bool weak = IsWeakOrdering(ordering);

  const std::size_t m = dependent.size();
  const std::size_t k = length;
  const std::size_t total = CountPermutations(m, k);
  if (total == 0) {
    Rcpp::stop(
        "There are too many permutations (length(dependent)^length) to "
        "enumerate.");
  }

  // classify every pair of dependent values once
  std::vector<int> classes(m * m);
  for (std::size_t a = 0; a < m; a++) {
    for (std::size_t b = 0; b < m; b++) {
      classes[a * m + b] = ImacClassify(dependent[a] - dependent[b],
                                        thresholds[0], thresholds[1], weak);
    }
  }

  arma::cube ordinals;
  if (k * (k - 1) / 2 <= kMaxPackedCells) {
    ordinals = UniqueMatrices<uint64_t>(classes, m, k, total);
  } else {
    ordinals = UniqueMatrices<std::string>(classes, m, k, total);
  }

  Rcpp::RObject permutations = R_NilValue;
  if (return_permutations) {
    if (total > static_cast<std::size_t>(INT_MAX)) {
      Rcpp::stop(
          "There are too many permutations to return; use "
          "'return_permutations = FALSE'.");
    }
    Rcpp::NumericMatrix out(static_cast<int>(total), static_cast<int>(k));
    for (std::size_t r = 0; r < total; r++) {
      std::size_t rest = r;
      for (std::size_t j = k; j-- > 0;) {
        out(r, j) = dependent[rest % m];
        rest /= m;
      }
    }
    permutations = out;
  }

  return Rcpp::List::create(
      Rcpp::Named("permutations") = permutations,
      Rcpp::Named("inequality_matrices") = ordinals,
      Rcpp::Named("n") = static_cast<double>(ordinals.n_slices));
}
