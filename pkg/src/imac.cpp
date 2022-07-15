// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <fstream>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
mat imac(vec probabilities, vec thresholds) {
  // setup environment
  mat inequality(probabilities.n_elem, probabilities.n_elem, fill::value(datum::nan));
  for (uword i = 0; i < probabilities.n_elem - 1; i++) {
    for (uword k = i + 1; k < probabilities.n_elem; k++) {
      inequality(i, k) =  probabilities[i] - probabilities[k];
    }
  }
  inequality.elem( find(inequality > thresholds[0])).ones();
  inequality.elem( find(inequality < thresholds[1])).fill(-1);
  inequality.elem( find(inequality < thresholds[0] && inequality > thresholds[1])).zeros();
  return(inequality);
}