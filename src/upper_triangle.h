// Copyright 2026 <Lenard Dome> [legal/copyright]
#ifndef CLOBE_SRC_UPPER_TRIANGLE_H_
#define CLOBE_SRC_UPPER_TRIANGLE_H_

#include <RcppArmadillo.h>

#include <cmath>
#include <string>

// Throws if a cell in the strict upper triangle of any slice of patterns is
// not finite (NA, NaN or infinite). Such cells cannot be compared: NaN is not
// equal to itself and breaks the ordering of sorted containers. Cells on and
// below the diagonal are ignored.
inline void CheckFiniteUpperTriangle(const arma::cube& patterns,
                                     const std::string& name) {
  for (arma::uword s = 0; s < patterns.n_slices; s++) {
    for (arma::uword col = 1; col < patterns.n_cols; col++) {
      for (arma::uword row = 0; row < col && row < patterns.n_rows; row++) {
        if (!std::isfinite(patterns(row, col, s))) {
          Rcpp::stop("'" + name +
                     "' must not have missing or infinite values above the "
                     "diagonal.");
        }
      }
    }
  }
}

#endif  // CLOBE_SRC_UPPER_TRIANGLE_H_
