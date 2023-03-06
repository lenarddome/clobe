// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// accomodation statistics
// compare two cubes of inequality matrices
// returns the complete list of unique ordinal matrices
colvec accomodation(cube discovered, cube predicted) {
  colvec accomodated(discovered.n_slices, fill::zeros);
  mat index(discovered.n_slices, predicted.n_slices);
  // carry out the comparisons
  for (int x = 0; x < discovered.n_slices; x++) {
    mat current = discovered.slice(x);
    for (int y = 0; y < predicted.n_slices; y++) {
      mat base = predicted.slice(y);
      umat result = (base == current);
      uvec comparisons = result(trimatu_ind(size(result), 1));
      index(x, y) =  all(comparisons == 1);
    }
    // if matrix has been found, set value to 1
    if (any(index.row(x) == 1)) {
      accomodated(x) = 1;
    }
  }
  colvec out = conv_to< colvec >::from(accomodated);
  return(out);
}

// prediction statistics
double prediction(cube discovered, cube predicted) {
  double prediction = 0;
  mat index(predicted.n_slices, discovered.n_slices);
  // carry out the comparisons
  for (int x = 0; x < predicted.n_slices; x++) {
    mat current = predicted.slice(x);
    for (int y = 0; y < discovered.n_slices; y++) {
      mat base = discovered.slice(y);
      umat result = (base == current);
      index(x, y) =  any(vectorise(result) == 0);
    }
    // if matrix has been found, set value to 1
    if (all(index.row(x) == 1)) {
      prediction += 1;
    }
  }
  return(prediction);
}

// [[Rcpp::export]]
List gdistance(arma::cube human, arma::cube model, double universal, double weight,
               arma::colvec frequencies, bool normalize = true,
              bool xtdo = false) {

  List out;

  vec alpha = accomodation(human, model);
  double alpha_weighted = sum(alpha % frequencies);

  double beta = prediction(human, model) / (universal - human.n_slices);

  if (!(beta > 0)) beta = 0;
  double distance = sqrt(weight * pow(1 - alpha_weighted, 2) +  (1 - weight) * pow(0 - beta, 2));

  if(normalize) distance = distance / sqrt(2);

  if (xtdo) {
    out = Rcpp::List::create(
      Rcpp::Named("gdistance") = distance,
      Rcpp::Named("alpha") = alpha_weighted,
      Rcpp::Named("beta") = beta,
      Rcpp::Named("accomodation") = wrap(find(alpha)));
  } else {
    out = Rcpp::List::create(
      Rcpp::Named("gdistance") = distance);
  }
  return(out);
}
