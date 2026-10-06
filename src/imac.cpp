// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "imac.h"

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

void imac_check_thresholds(const arma::vec& thresholds) {
  if (thresholds.n_elem != 2) {
    stop("'thresholds' must have exactly 2 elements: c(upper, lower).");
  }
  if (thresholds[0] < thresholds[1]) {
    stop("'thresholds' must be c(upper, lower) with upper >= lower.");
  }
}

bool imac_is_weak(const std::string& ordering) {
  if (ordering == "semiorder") return false;
  if (ordering == "weak") return true;
  stop("'ordering' must be either \"semiorder\" or \"weak\".");
}

// [[Rcpp::export]]
arma::mat imac(arma::vec probabilities, arma::vec thresholds,
               std::string ordering = "semiorder") {
  imac_check_thresholds(thresholds);
  const bool weak = imac_is_weak(ordering);
  // cells on and below the diagonal are not compared and take the value 2
  mat inequality(probabilities.n_elem, probabilities.n_elem, fill::value(2));
  for (uword i = 0; i < probabilities.n_elem; i++) {
    for (uword k = i + 1; k < probabilities.n_elem; k++) {
      inequality(i, k) = imac_classify(probabilities[i] - probabilities[k],
                                       thresholds[0], thresholds[1], weak);
    }
  }
  return(inequality);
}
