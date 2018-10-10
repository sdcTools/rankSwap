// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// recordSwap_cpp
std::vector< std::vector<int> > recordSwap_cpp(std::vector< std::vector<int> > data, std::vector<int> similar, std::vector<int> hierarchy, std::vector<int> risk, int hid, int th, double swaprate, int seed);
RcppExport SEXP _recordSwapping_recordSwap_cpp(SEXP dataSEXP, SEXP similarSEXP, SEXP hierarchySEXP, SEXP riskSEXP, SEXP hidSEXP, SEXP thSEXP, SEXP swaprateSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::vector<int> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type similar(similarSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type hierarchy(hierarchySEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type risk(riskSEXP);
    Rcpp::traits::input_parameter< int >::type hid(hidSEXP);
    Rcpp::traits::input_parameter< int >::type th(thSEXP);
    Rcpp::traits::input_parameter< double >::type swaprate(swaprateSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(recordSwap_cpp(data, similar, hierarchy, risk, hid, th, swaprate, seed));
    return rcpp_result_gen;
END_RCPP
}
// setLevels_cpp
std::vector<int> setLevels_cpp(std::vector< std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk, int hid, int th);
RcppExport SEXP _recordSwapping_setLevels_cpp(SEXP dataSEXP, SEXP hierarchySEXP, SEXP riskSEXP, SEXP hidSEXP, SEXP thSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::vector<int> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type hierarchy(hierarchySEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type risk(riskSEXP);
    Rcpp::traits::input_parameter< int >::type hid(hidSEXP);
    Rcpp::traits::input_parameter< int >::type th(thSEXP);
    rcpp_result_gen = Rcpp::wrap(setLevels_cpp(data, hierarchy, risk, hid, th));
    return rcpp_result_gen;
END_RCPP
}
// orderData_cpp
std::vector< std::vector<int> > orderData_cpp(std::vector< std::vector<int> >& data, int orderIndex);
RcppExport SEXP _recordSwapping_orderData_cpp(SEXP dataSEXP, SEXP orderIndexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::vector<int> >& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type orderIndex(orderIndexSEXP);
    rcpp_result_gen = Rcpp::wrap(orderData_cpp(data, orderIndex));
    return rcpp_result_gen;
END_RCPP
}
// test_randSample_cpp
std::vector<std::vector<int> > test_randSample_cpp(int B, std::vector<int> ID, int N, std::vector<double> prob, int seed);
RcppExport SEXP _recordSwapping_test_randSample_cpp(SEXP BSEXP, SEXP IDSEXP, SEXP NSEXP, SEXP probSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type ID(IDSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type prob(probSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(test_randSample_cpp(B, ID, N, prob, seed));
    return rcpp_result_gen;
END_RCPP
}
// setRisk_cpp
std::vector< std::vector<double> > setRisk_cpp(std::vector<std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk, int hid);
RcppExport SEXP _recordSwapping_setRisk_cpp(SEXP dataSEXP, SEXP hierarchySEXP, SEXP riskSEXP, SEXP hidSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::vector<int> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type hierarchy(hierarchySEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type risk(riskSEXP);
    Rcpp::traits::input_parameter< int >::type hid(hidSEXP);
    rcpp_result_gen = Rcpp::wrap(setRisk_cpp(data, hierarchy, risk, hid));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_recordSwapping_recordSwap_cpp", (DL_FUNC) &_recordSwapping_recordSwap_cpp, 8},
    {"_recordSwapping_setLevels_cpp", (DL_FUNC) &_recordSwapping_setLevels_cpp, 5},
    {"_recordSwapping_orderData_cpp", (DL_FUNC) &_recordSwapping_orderData_cpp, 2},
    {"_recordSwapping_test_randSample_cpp", (DL_FUNC) &_recordSwapping_test_randSample_cpp, 5},
    {"_recordSwapping_setRisk_cpp", (DL_FUNC) &_recordSwapping_setRisk_cpp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_recordSwapping(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
