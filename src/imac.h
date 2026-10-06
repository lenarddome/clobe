#ifndef IMAC_H
#define IMAC_H

#include <RcppArmadillo.h>

// absolute tolerance used when comparing a difference against a threshold,
// so that e.g. 0.3 - 0.2 (= 0.0999...) counts as reaching a threshold of 0.1
const double IMAC_TOLERANCE = 1.490116119384765625e-08;  // sqrt(DBL_EPSILON)

// classify a single difference as 1 (larger), -1 (smaller) or 0 (approximately
// equal); upper and lower are thresholds[0] and thresholds[1]
inline int imac_classify(double difference, double upper, double lower) {
  if (difference >= upper - IMAC_TOLERANCE) return 1;
  if (difference <= lower + IMAC_TOLERANCE) return -1;
  return 0;
}

// throws if thresholds are not c(upper, lower) with upper >= lower
void imac_check_thresholds(const arma::vec& thresholds);

arma::mat imac(arma::vec probabilities, arma::vec thresholds);

#endif
