// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include "imac.h"

#include <RcppArmadillo.h>

#include <string>

void CheckThresholds(const arma::vec& thresholds) {
  if (thresholds.n_elem != 2) {
    Rcpp::stop("'thresholds' must have exactly 2 elements: c(upper, lower).");
  }
  if (thresholds[0] < thresholds[1]) {
    Rcpp::stop("'thresholds' must be c(upper, lower) with upper >= lower.");
  }
}

bool IsWeakOrdering(const std::string& ordering) {
  if (ordering == "semiorder") return false;
  if (ordering == "weak") return true;
  Rcpp::stop("'ordering' must be either \"semiorder\" or \"weak\".");
}

// [[Rcpp::export(name = "imac")]]
arma::mat Imac(const arma::vec& probabilities, const arma::vec& thresholds,
               const std::string& ordering = "semiorder") {
  CheckThresholds(thresholds);
  const bool weak = IsWeakOrdering(ordering);
  // Cells on and below the diagonal are not compared and take the value 2.
  arma::mat inequality(probabilities.n_elem, probabilities.n_elem,
                       arma::fill::value(2));
  for (arma::uword i = 0; i < probabilities.n_elem; i++) {
    for (arma::uword k = i + 1; k < probabilities.n_elem; k++) {
      inequality(i, k) = ImacClassify(probabilities[i] - probabilities[k],
                                      thresholds[0], thresholds[1], weak);
    }
  }
  return inequality;
}
