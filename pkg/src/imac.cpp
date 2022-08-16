// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::mat imac(arma::vec probabilities, arma::vec thresholds) {
  // setup environment
  mat inequality(probabilities.n_elem, probabilities.n_elem, fill::value(2));
  for (uword i = 0; i < probabilities.n_elem; i++) {
    for (uword k = i + 1; k < probabilities.n_elem; k++) {
      inequality(i, k) =  probabilities[i] - probabilities[k];
    }
  }
  // apply the rules to the differences
  inequality.elem( find(inequality > thresholds[0] && inequality < 2)).fill(1);
  inequality.elem( find(inequality < thresholds[1])).fill(-1);
  inequality.elem( find(inequality > thresholds[1] && inequality < thresholds[0])).fill(0);
  return(inequality);
}
