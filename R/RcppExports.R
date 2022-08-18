# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

brutes <- function(probabilities, length, thresholds) {
    .Call(`_globe_brutes`, probabilities, length, thresholds)
}

gdistance <- function(human, model, universal, weight, frequencies, xtdo = FALSE) {
    .Call(`_globe_gdistance`, human, model, universal, weight, frequencies, xtdo)
}

imac <- function(probabilities, thresholds) {
    .Call(`_globe_imac`, probabilities, thresholds)
}
