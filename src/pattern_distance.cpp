// Copyright 2026 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include <cmath>
#include <string>
#include <vector>

#include "upper_triangle.h"

namespace {

// Returns the strict upper triangles of the slices of patterns, one column
// per slice.
arma::mat UpperTriangles(const arma::cube& patterns) {
  const arma::uword k = patterns.n_rows;
  arma::mat cells(k * (k - 1) / 2, patterns.n_slices);
  for (arma::uword s = 0; s < patterns.n_slices; s++) {
    arma::uword c = 0;
    for (arma::uword i = 0; i < k; i++) {
      for (arma::uword j = i + 1; j < k; j++) cells(c++, s) = patterns(i, j, s);
    }
  }
  return cells;
}

}  // namespace

// [[Rcpp::export(name = ".pattern_distance")]]
arma::mat PatternDistance(const arma::cube& x, const arma::cube& y,
                          const std::string& method) {
  if (method != "kemeny" && method != "hamming") {
    Rcpp::stop("'method' must be either \"kemeny\" or \"hamming\".");
  }
  CheckFiniteUpperTriangle(x, "x");
  CheckFiniteUpperTriangle(y, "y");
  const bool kemeny = method == "kemeny";
  const arma::mat a = UpperTriangles(x);
  const arma::mat b = UpperTriangles(y);
  arma::mat distance(a.n_cols, b.n_cols);
  for (arma::uword s = 0; s < a.n_cols; s++) {
    for (arma::uword t = 0; t < b.n_cols; t++) {
      double total = 0;
      for (arma::uword c = 0; c < a.n_rows; c++) {
        const double difference = std::fabs(a(c, s) - b(c, t));
        total += kemeny ? difference : (difference != 0);
      }
      distance(s, t) = total;
    }
  }
  return distance;
}
