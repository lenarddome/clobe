// Copyright 2022 <Lenard Dome> [legal/copyright]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include <cmath>
#include <set>
#include <vector>

#include "upper_triangle.h"

namespace {

// Two inequality matrices describe the same pattern if their strict upper
// triangles are identical; the diagonal and lower triangle carry no
// information.
bool SamePattern(const arma::mat& a, const arma::mat& b) {
  for (arma::uword col = 1; col < a.n_cols; col++) {
    for (arma::uword row = 0; row < col; row++) {
      if (a(row, col) != b(row, col)) return false;
    }
  }
  return true;
}

// Accommodation statistics: returns 1 for each pattern in discovered that is
// also in predicted, and 0 otherwise.
arma::colvec Accommodation(const arma::cube& discovered,
                           const arma::cube& predicted) {
  arma::colvec accommodated(discovered.n_slices, arma::fill::zeros);
  for (arma::uword x = 0; x < discovered.n_slices; x++) {
    for (arma::uword y = 0; y < predicted.n_slices; y++) {
      if (SamePattern(discovered.slice(x), predicted.slice(y))) {
        accommodated(x) = 1;
        break;
      }
    }
  }
  return accommodated;
}

// Returns the unique patterns in a cube, each as its strict upper triangle.
std::set<std::vector<double>> UniquePatterns(const arma::cube& patterns) {
  std::set<std::vector<double>> unique;
  std::vector<double> cells;
  for (arma::uword s = 0; s < patterns.n_slices; s++) {
    cells.clear();
    for (arma::uword col = 1; col < patterns.n_cols; col++) {
      for (arma::uword row = 0; row < col; row++) {
        cells.push_back(patterns(row, col, s));
      }
    }
    unique.insert(cells);
  }
  return unique;
}

// Prediction statistics: counts the unique patterns in predicted that are not
// in discovered.
double Prediction(const std::set<std::vector<double>>& discovered,
                  const std::set<std::vector<double>>& predicted) {
  double prediction = 0;
  for (const std::vector<double>& pattern : predicted) {
    if (discovered.count(pattern) == 0) prediction += 1;
  }
  return prediction;
}

}  // namespace

// [[Rcpp::export(name = "gdistance")]]
Rcpp::List GDistance(const arma::cube& human, const arma::cube& model,
                     double universal, double weight,
                     const arma::colvec& frequencies, bool xtdo = false) {
  if (human.n_rows != model.n_rows || human.n_cols != model.n_cols ||
      human.n_rows != human.n_cols) {
    Rcpp::stop(
        "'human' and 'model' must contain square matrices of the same "
        "dimensions.");
  }
  CheckFiniteUpperTriangle(human, "human");
  CheckFiniteUpperTriangle(model, "model");
  if (frequencies.n_elem != human.n_slices) {
    Rcpp::stop("'frequencies' must have one element per pattern in 'human'.");
  }
  const std::set<std::vector<double>> human_patterns = UniquePatterns(human);
  const double observed = human_patterns.size();
  if (universal < observed) {
    Rcpp::stop(
        "'universal' cannot be smaller than the number of unique patterns in "
        "'human'.");
  }
  if (weight < 0 || weight > 1) {
    Rcpp::stop("'weight' must be between 0 and 1.");
  }

  const arma::vec alpha = Accommodation(human, model);
  const double alpha_weighted = arma::sum(alpha % frequencies);

  double beta;
  double distance;
  // if human = universal, there are no predictions to be made
  if (universal == observed) {
    // only calculate accommodation and return warning
    beta = arma::datum::nan;
    distance = std::sqrt(weight * std::pow(1 - alpha_weighted, 2));
    Rcpp::warning(
        "'beta' cannot be calculated because human = universal, so it was "
        "excluded from distance.");
  } else {
    beta = Prediction(human_patterns, UniquePatterns(model)) /
           (universal - observed);
    distance = std::sqrt(weight * std::pow(1 - alpha_weighted, 2) +
                         (1 - weight) * std::pow(beta, 2));
  }

  if (!xtdo) return Rcpp::List::create(Rcpp::Named("gdistance") = distance);

  // R indices are 1-based
  const arma::uvec accommodated = arma::find(alpha) + 1;
  return Rcpp::List::create(
      Rcpp::Named("gdistance") = distance,
      Rcpp::Named("alpha") = alpha_weighted, Rcpp::Named("beta") = beta,
      Rcpp::Named("accommodation") =
          Rcpp::IntegerVector(accommodated.begin(), accommodated.end()));
}
