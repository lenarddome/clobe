// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// brutes
List brutes(NumericVector probabilities, int length, arma::vec thresholds);
RcppExport SEXP _clobe_brutes(SEXP probabilitiesSEXP, SEXP lengthSEXP, SEXP thresholdsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type probabilities(probabilitiesSEXP);
    Rcpp::traits::input_parameter< int >::type length(lengthSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type thresholds(thresholdsSEXP);
    rcpp_result_gen = Rcpp::wrap(brutes(probabilities, length, thresholds));
    return rcpp_result_gen;
END_RCPP
}
// gdistance
List gdistance(arma::cube human, arma::cube model, double universal, double weight, arma::colvec frequencies, bool xtdo);
RcppExport SEXP _clobe_gdistance(SEXP humanSEXP, SEXP modelSEXP, SEXP universalSEXP, SEXP weightSEXP, SEXP frequenciesSEXP, SEXP xtdoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube >::type human(humanSEXP);
    Rcpp::traits::input_parameter< arma::cube >::type model(modelSEXP);
    Rcpp::traits::input_parameter< double >::type universal(universalSEXP);
    Rcpp::traits::input_parameter< double >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type frequencies(frequenciesSEXP);
    Rcpp::traits::input_parameter< bool >::type xtdo(xtdoSEXP);
    rcpp_result_gen = Rcpp::wrap(gdistance(human, model, universal, weight, frequencies, xtdo));
    return rcpp_result_gen;
END_RCPP
}
// imac
arma::mat imac(arma::vec probabilities, arma::vec thresholds);
RcppExport SEXP _clobe_imac(SEXP probabilitiesSEXP, SEXP thresholdsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type probabilities(probabilitiesSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type thresholds(thresholdsSEXP);
    rcpp_result_gen = Rcpp::wrap(imac(probabilities, thresholds));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_clobe_brutes", (DL_FUNC) &_clobe_brutes, 3},
    {"_clobe_gdistance", (DL_FUNC) &_clobe_gdistance, 6},
    {"_clobe_imac", (DL_FUNC) &_clobe_imac, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_clobe(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
