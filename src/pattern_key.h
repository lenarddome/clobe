// Copyright 2026 <Lenard Dome> [legal/copyright]
#ifndef CLOBE_SRC_PATTERN_KEY_H_
#define CLOBE_SRC_PATTERN_KEY_H_

#include <RcppArmadillo.h>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <string>
#include <vector>

// Every inequality matrix is identified by its strict upper triangle, read
// row by row, where each cell is one of -1, 0, 1. The triangle is stored as a
// key: a base-3 number when it has at most kMaxPackedCells cells
// (3^40 < 2^64), otherwise a string with one character per cell. Keys of the
// same type and length sort in the same order.
constexpr std::size_t kMaxPackedCells = 40;

// Returns the number of cells in the strict upper triangle of a k x k matrix.
inline std::size_t CountCells(std::size_t k) { return k * (k - 1) / 2; }

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

// A set of keys. Keys are buffered and periodically sorted and merged into a
// sorted vector of unique keys, which avoids an allocation per key.
template <typename Key>
class KeySet {
 public:
  void Add(const Key& key) {
    // consecutive keys are often equal
    if (!buffer_.empty() && buffer_.back() == key) return;
    buffer_.push_back(key);
    if (buffer_.size() >= std::max(kMinBuffer, unique_.size())) Flush();
  }

  // Moves the keys added so far into a sorted vector of unique keys.
  std::vector<Key> TakeSorted() {
    Flush();
    std::vector<Key> keys;
    keys.swap(unique_);
    return keys;
  }

 private:
  static constexpr std::size_t kMinBuffer = std::size_t{1} << 16;

  // Merges the buffered keys into unique_.
  void Flush() {
    if (buffer_.empty()) return;
    std::sort(buffer_.begin(), buffer_.end());
    buffer_.erase(std::unique(buffer_.begin(), buffer_.end()), buffer_.end());
    std::vector<Key> merged;
    merged.reserve(unique_.size() + buffer_.size());
    std::set_union(unique_.begin(), unique_.end(), buffer_.begin(),
                   buffer_.end(), std::back_inserter(merged));
    unique_.swap(merged);
    buffer_.clear();
  }

  std::vector<Key> unique_;  // sorted
  std::vector<Key> buffer_;
};

// Returns the k x k inequality matrices of keys as a cube, with 2 on and
// below the diagonal.
template <typename Key>
arma::cube KeysToCube(const std::vector<Key>& keys, std::size_t k) {
  arma::cube ordinals(k, k, keys.size());
  ordinals.fill(2);
  std::vector<int> cells(CountCells(k));
  for (arma::uword s = 0; s < keys.size(); s++) {
    KeyDecode(keys[s], &cells);
    std::size_t c = 0;
    for (arma::uword i = 0; i < k; i++) {
      for (arma::uword j = i + 1; j < k; j++) ordinals(i, j, s) = cells[c++];
    }
  }
  return ordinals;
}

#endif  // CLOBE_SRC_PATTERN_KEY_H_
