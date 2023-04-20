// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "imac.h"

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// compare two cubes of inequality matrices
// returns the complete list of unique ordinal matrices
cube OrdinalCompare(cube discovered, cube predicted) {

  cube drawer(discovered);
  mat index(predicted.n_slices, discovered.n_slices);

  // carry out the comparisons
  for (int x = 0; x < predicted.n_slices; x++) {
    mat current = predicted.slice(x);
    for (int y = 0; y < discovered.n_slices; y++) {
      mat base = discovered.slice(y);
      umat result = (base == current);
      index(x, y) =  any(vectorise(result) == 0);
    }

    if (all(index.row(x) == 1)) {
      cube update = join_slices(drawer, current);
      drawer = update;
    }
  }
  return(drawer);
}


// [[Rcpp::export]]
List brutes(NumericVector dependent, int length, arma::vec thresholds) {

    // Obtaining namespace of RcppAlgos package
    Environment pkg = Environment::namespace_env("RcppAlgos");

    // Picking up permuteGeneral function from Matrix package
    Function Permutation = pkg["permuteGeneral"];

    NumericMatrix permutations = Permutation(Named("v", dependent),
                                             Named("m", length),
                                             Named("repetition", true));
    const mat& permutes = as<mat>(permutations);


    cube ordinals(length, length, 1, fill::zeros);

    for (uword i = 0; i < permutes.n_rows; i++) {\
        cube predictions(length, length, 1, fill::zeros);

        mat evaluate = imac(permutes.row(i).as_col(), thresholds);

        predictions.slice(0) = evaluate;

        if (i == 0) {
            ordinals = predictions;
        } else {
            ordinals = OrdinalCompare(ordinals, predictions);
        }
    }

    List out = List::create(
      Named("permutations") = permutes,
      Named("inequality_matrices") = ordinals,
      Named("n") = ordinals.n_slices);

    return(out);
}
