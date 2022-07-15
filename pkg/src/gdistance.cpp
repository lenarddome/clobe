// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <fstream>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// accomodation statistics
// compare two cubes of inequality matrices
// returns the complete list of unique ordinal matrices
vec alpha(cube discovered, cube predicted) {
  vec accomodated(discovered.n_slices, fill::zeros);
  mat index(predicted.n_slices, discovered.n_slices);
  // carry out the comparisons
  for (int x = 0; x < predicted.n_slices; x++) {
    mat current = predicted.slice(x);
    for (int y = 0; y < discovered.n_slices; y++) {
      mat base = discovered.slice(y);
      umat result = (base == current);
      uvec comparisons = result(trimatu_ind(size(result), 1));
      index(x, y) =  any(comparisons == 0);
    }
    // if matrix has been found, set value to 1
    if (all(index.row(x) == 1)) {
      accomodated(x) = 1;
    }
  }
  return(accomodated);
}

// prediction statistics
double beta(cube discovered, cube predicted) {
  double prediction = 0;
  mat index(predicted.n_slices, discovered.n_slices);
  // carry out the comparisons
  for (int x = 0; x < predicted.n_slices; x++) {
    mat current = predicted.slice(x);
    for (int y = 0; y < discovered.n_slices; y++) {
      mat base = discovered.slice(y);
      umat result = (base == current);
      uvec comparisons = result(trimatu_ind(size(result), 1));
      index(x, y) =  any(comparisons == 0);
    }
    // if matrix has been found, set value to 1
    if (all(index.row(x) == 1)) {
      prediction += 1;
    }
  }
  return(prediction);
}

// weighting accomodation
double alphaWeighted(vec accomodated, vec frequencies) {
  return(alpha_weighted)
}

// weighted euclidean distance

// [[Rcpp::export]]
List gdistance(cube human, cube model, double universal, double weights, vec frequencies, bool xtdo = false) {

}
