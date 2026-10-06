// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo, RcppParallel)]]
#include <RcppArmadillo.h>
#include <RcppParallel.h>

#include <algorithm>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <iterator>
#include <string>
#include <thread>
#include <vector>

#include "imac.h"
#include "pattern_key.h"

namespace {

// Permutations are processed in chunks of this size, checking for a user
// interrupt between chunks; worker threads cannot call the R API.
constexpr std::size_t kChunkSize = std::size_t{1} << 24;

// Collects the unique inequality matrices produced by ranges of
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
      found_.Add(key);
      // advance to the next permutation
      for (std::size_t j = k_; j-- > 0;) {
        if (++digits[j] < m_) break;
        digits[j] = 0;
      }
    }
  }

  // Moves the patterns found so far into a sorted vector.
  std::vector<Key> TakeSorted() { return found_.TakeSorted(); }

 private:
  const std::vector<int>& classes_;
  std::size_t m_;
  std::size_t k_;
  KeySet<Key> found_;
};

// Splits permutations [begin, end) into one slice per collector, so that
// each thread adds to its own collector and no merging is needed per chunk.
template <typename Key>
class SliceWorker : public RcppParallel::Worker {
 public:
  SliceWorker(std::vector<Collector<Key>>* collectors, std::size_t begin,
              std::size_t end)
      : collectors_(collectors), begin_(begin), end_(end) {}

  void operator()(std::size_t from, std::size_t to) override {
    const std::size_t length = end_ - begin_;
    const std::size_t slices = collectors_->size();
    for (std::size_t t = from; t < to; t++) {
      (*collectors_)[t].Collect(begin_ + length * t / slices,
                                begin_ + length * (t + 1) / slices);
    }
  }

 private:
  std::vector<Collector<Key>>* collectors_;
  std::size_t begin_;
  std::size_t end_;
};

// Turns each collector's patterns into a sorted vector.
template <typename Key>
class SortWorker : public RcppParallel::Worker {
 public:
  SortWorker(std::vector<Collector<Key>>* collectors,
             std::vector<std::vector<Key>>* keys)
      : collectors_(collectors), keys_(keys) {}

  void operator()(std::size_t from, std::size_t to) override {
    for (std::size_t t = from; t < to; t++) {
      (*keys_)[t] = (*collectors_)[t].TakeSorted();
    }
  }

 private:
  std::vector<Collector<Key>>* collectors_;
  std::vector<std::vector<Key>>* keys_;
};

// Merges sorted vector i + step into sorted vector i, for every i that is a
// multiple of 2 * step.
template <typename Key>
class MergeWorker : public RcppParallel::Worker {
 public:
  MergeWorker(std::vector<std::vector<Key>>* keys, std::size_t step)
      : keys_(keys), step_(step) {}

  void operator()(std::size_t from, std::size_t to) override {
    for (std::size_t pair = from; pair < to; pair++) {
      std::vector<Key>& left = (*keys_)[pair * 2 * step_];
      std::vector<Key>& right = (*keys_)[pair * 2 * step_ + step_];
      std::vector<Key> merged;
      merged.reserve(left.size() + right.size());
      std::set_union(left.begin(), left.end(), right.begin(), right.end(),
                     std::back_inserter(merged));
      left.swap(merged);
      std::vector<Key>().swap(right);
    }
  }

 private:
  std::vector<std::vector<Key>>* keys_;
  std::size_t step_;
};

// Returns the number of threads to use: threads if positive, otherwise the
// RCPP_PARALLEL_NUM_THREADS environment variable (set by
// RcppParallel::setThreadOptions), otherwise the number of cores.
int ResolveThreads(int threads) {
  if (threads > 0) return threads;
  const char* option = std::getenv("RCPP_PARALLEL_NUM_THREADS");
  if (option != nullptr && std::atoi(option) > 0) return std::atoi(option);
  const unsigned int cores = std::thread::hardware_concurrency();
  return cores > 0 ? static_cast<int>(cores) : 1;
}

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
                          std::size_t k, std::size_t total, int threads) {
  const std::size_t slices = ResolveThreads(threads);
  std::vector<Collector<Key>> collectors(slices, Collector<Key>(classes, m, k));
  for (std::size_t begin = 0; begin < total; begin += kChunkSize) {
    const std::size_t end = begin + std::min(kChunkSize, total - begin);
    SliceWorker<Key> slice_worker(&collectors, begin, end);
    RcppParallel::parallelFor(0, slices, slice_worker, 1, slices);
    Rcpp::checkUserInterrupt();
  }

  // sort and merge, so that the output does not depend on the order of
  // discovery
  std::vector<std::vector<Key>> sorted(slices);
  SortWorker<Key> sort_worker(&collectors, &sorted);
  RcppParallel::parallelFor(0, slices, sort_worker, 1, slices);
  for (std::size_t step = 1; step < slices; step *= 2) {
    const std::size_t pairs = (slices - step + 2 * step - 1) / (2 * step);
    MergeWorker<Key> merge_worker(&sorted, step);
    RcppParallel::parallelFor(0, pairs, merge_worker, 1, slices);
  }
  return KeysToCube(sorted[0], k);
}

}  // namespace

// [[Rcpp::export(name = "brutes")]]
Rcpp::List Brutes(const Rcpp::NumericVector& dependent, int length,
                  const arma::vec& thresholds,
                  const std::string& ordering = "semiorder",
                  bool return_permutations = false, int threads = -1) {
  if (dependent.size() == 0) Rcpp::stop("'dependent' must not be empty.");
  if (length < 1) Rcpp::stop("'length' must be at least 1.");
  CheckThresholds(thresholds);
  const bool weak = IsWeakOrdering(ordering);
  if (threads != -1 && threads < 1) {
    Rcpp::stop("'threads' must be -1 (the default) or at least 1.");
  }

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
  if (CountCells(k) <= kMaxPackedCells) {
    ordinals = UniqueMatrices<uint64_t>(classes, m, k, total, threads);
  } else {
    ordinals = UniqueMatrices<std::string>(classes, m, k, total, threads);
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
