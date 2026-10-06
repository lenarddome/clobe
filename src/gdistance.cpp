// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// two inequality matrices describe the same pattern if their strict upper
// triangles are identical; the diagonal and lower triangle carry no information
bool same_pattern(const mat& a, const mat& b) {
  for (uword col = 1; col < a.n_cols; col++) {
    for (uword row = 0; row < col; row++) {
      if (a(row, col) != b(row, col)) return false;
    }
  }
  return true;
}

// accommodation statistics
// returns 1 for each pattern in discovered that is also in predicted, else 0
colvec accommodation(const cube& discovered, const cube& predicted) {
  colvec accommodated(discovered.n_slices, fill::zeros);
  for (uword x = 0; x < discovered.n_slices; x++) {
    for (uword y = 0; y < predicted.n_slices; y++) {
      if (same_pattern(discovered.slice(x), predicted.slice(y))) {
        accommodated(x) = 1;
        break;
      }
    }
  }
  return(accommodated);
}

// prediction statistics
// counts the patterns in predicted that are not in discovered
double prediction(const cube& discovered, const cube& predicted) {
  double prediction = 0;
  for (uword x = 0; x < predicted.n_slices; x++) {
    bool found = false;
    for (uword y = 0; y < discovered.n_slices && !found; y++) {
      found = same_pattern(discovered.slice(y), predicted.slice(x));
    }
    if (!found) prediction += 1;
  }
  return(prediction);
}

// [[Rcpp::export]]
List gdistance(const arma::cube& human, const arma::cube& model,
               double universal,
               double weight,
               arma::colvec frequencies,
               bool xtdo = false) {

  if (human.n_rows != model.n_rows || human.n_cols != model.n_cols ||
      human.n_rows != human.n_cols) {
    stop("'human' and 'model' must contain square matrices of the same "
         "dimensions.");
  }
  if (frequencies.n_elem != human.n_slices) {
    stop("'frequencies' must have one element per pattern in 'human'.");
  }
  if (universal < human.n_slices) {
    stop("'universal' cannot be smaller than the number of patterns in "
         "'human'.");
  }
  if (weight < 0 || weight > 1) {
    stop("'weight' must be between 0 and 1.");
  }

  double alpha_weighted, beta, distance;
  bool undefined;
  List out;

  vec alpha = accommodation(human, model);
  alpha_weighted = sum(alpha % frequencies);

  // if there are no prediction to be made, set undefined to true
  undefined = (universal == human.n_slices);

  if (undefined) {
    // if human = universal, only calculate accommodation and return warning
    beta = datum::nan;
    distance = sqrt(weight * pow(1 - alpha_weighted, 2));
    Rcpp::warning("'beta' cannot be calculated because human = universal, "
                  "so it was excluded from distance.");
  } else {
    beta = prediction(human, model) / (universal - human.n_slices);
    distance = sqrt(weight * pow(1 - alpha_weighted, 2) + (1 - weight) * pow(beta, 2));
  }

  if (xtdo) {
    // R indices are 1-based
    uvec accommodated = find(alpha) + 1;
    out = Rcpp::List::create(
      Rcpp::Named("gdistance") = distance,
      Rcpp::Named("alpha") = alpha_weighted,
      Rcpp::Named("beta") = beta,
      Rcpp::Named("accommodation") =
        IntegerVector(accommodated.begin(), accommodated.end()));
  } else {
    out = Rcpp::List::create(
      Rcpp::Named("gdistance") = distance);
  }
  return(out);
}
