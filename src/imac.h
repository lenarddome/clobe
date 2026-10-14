// Copyright 2022 <Lenard Dome> [legal/copyright]
#ifndef CLOBE_SRC_IMAC_H_
#define CLOBE_SRC_IMAC_H_

#include <RcppArmadillo.h>

#include <string>

// Absolute tolerance used when comparing a difference against a threshold,
// so that e.g. 0.3 - 0.2 (= 0.0999...) and 0.2 - 0.1 are treated alike.
constexpr double kImacTolerance =
    1.490116119384765625e-08;  // sqrt(DBL_EPSILON)

// Classifies a single difference as 1 (larger), -1 (smaller) or 0
// (approximately equal); upper and lower are thresholds[0] and thresholds[1].
// semiorder: approximately equal if lower <= difference <= upper
// weak:      approximately equal only if the difference is 0 (a tie)
inline int ImacClassify(double difference, double upper, double lower,
                        bool weak) {
  if (weak) {
    upper = 0;
    lower = 0;
  }
  if (difference > upper + kImacTolerance) return 1;
  if (difference < lower - kImacTolerance) return -1;
  return 0;
}

// Throws if thresholds are not c(upper, lower) with upper >= lower.
void CheckThresholds(const arma::vec& thresholds);

// Returns true for "weak", false for "semiorder", and throws otherwise.
bool IsWeakOrdering(const std::string& ordering);

arma::mat Imac(const arma::vec& probabilities, const arma::vec& thresholds,
               const std::string& ordering);

#endif  // CLOBE_SRC_IMAC_H_
