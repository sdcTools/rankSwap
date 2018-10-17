// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// recordSwap_cpp
std::vector< std::vector<int> > recordSwap_cpp(std::vector< std::vector<int> > data, std::vector<std::vector<int>> similar, std::vector<int> hierarchy, std::vector<int> risk_variables, int hid, int k_anonymity, double swaprate, double risk_threshold, std::vector<std::vector<double>> risk, int seed);
RcppExport SEXP _recordSwapping_recordSwap_cpp(SEXP dataSEXP, SEXP similarSEXP, SEXP hierarchySEXP, SEXP risk_variablesSEXP, SEXP hidSEXP, SEXP k_anonymitySEXP, SEXP swaprateSEXP, SEXP risk_thresholdSEXP, SEXP riskSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::vector<int> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector<std::vector<int>> >::type similar(similarSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type hierarchy(hierarchySEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type risk_variables(risk_variablesSEXP);
    Rcpp::traits::input_parameter< int >::type hid(hidSEXP);
    Rcpp::traits::input_parameter< int >::type k_anonymity(k_anonymitySEXP);
    Rcpp::traits::input_parameter< double >::type swaprate(swaprateSEXP);
    Rcpp::traits::input_parameter< double >::type risk_threshold(risk_thresholdSEXP);
    Rcpp::traits::input_parameter< std::vector<std::vector<double>> >::type risk(riskSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(recordSwap_cpp(data, similar, hierarchy, risk_variables, hid, k_anonymity, swaprate, risk_threshold, risk, seed));
    return rcpp_result_gen;
END_RCPP
}
// setLevels_cpp
std::vector<int> setLevels_cpp(std::vector< std::vector<double> > risk, double risk_threshold);
RcppExport SEXP _recordSwapping_setLevels_cpp(SEXP riskSEXP, SEXP risk_thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type risk(riskSEXP);
    Rcpp::traits::input_parameter< double >::type risk_threshold(risk_thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(setLevels_cpp(risk, risk_threshold));
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
std::vector<int> test_randSample_cpp(std::vector<int> ID, int N, std::vector<double> prob, std::vector<int> IDused, int seed);
RcppExport SEXP _recordSwapping_test_randSample_cpp(SEXP IDSEXP, SEXP NSEXP, SEXP probSEXP, SEXP IDusedSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type ID(IDSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type prob(probSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type IDused(IDusedSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(test_randSample_cpp(ID, N, prob, IDused, seed));
    return rcpp_result_gen;
END_RCPP
}
// setRisk_cpp
std::vector< std::vector<double> > setRisk_cpp(std::vector<std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk_variables, int hid);
RcppExport SEXP _recordSwapping_setRisk_cpp(SEXP dataSEXP, SEXP hierarchySEXP, SEXP risk_variablesSEXP, SEXP hidSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::vector<int> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type hierarchy(hierarchySEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type risk_variables(risk_variablesSEXP);
    Rcpp::traits::input_parameter< int >::type hid(hidSEXP);
    rcpp_result_gen = Rcpp::wrap(setRisk_cpp(data, hierarchy, risk_variables, hid));
    return rcpp_result_gen;
END_RCPP
}
// test_distributeDraws_cpp
std::vector<std::vector<int>> test_distributeDraws_cpp(std::vector< std::vector<int> > data, std::vector<int> hierarchy, int hid, double swaprate, int seed);
RcppExport SEXP _recordSwapping_test_distributeDraws_cpp(SEXP dataSEXP, SEXP hierarchySEXP, SEXP hidSEXP, SEXP swaprateSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::vector<int> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type hierarchy(hierarchySEXP);
    Rcpp::traits::input_parameter< int >::type hid(hidSEXP);
    Rcpp::traits::input_parameter< double >::type swaprate(swaprateSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(test_distributeDraws_cpp(data, hierarchy, hid, swaprate, seed));
    return rcpp_result_gen;
END_RCPP
}
// test_sampleDonor_cpp
std::vector<int> test_sampleDonor_cpp(std::vector< std::vector<int> > data, std::vector<std::vector<int>> similar, int hid, std::vector<int> IDswap_vec, std::vector<int> IDswap_pool_vec, std::vector<double> prob, int seed);
RcppExport SEXP _recordSwapping_test_sampleDonor_cpp(SEXP dataSEXP, SEXP similarSEXP, SEXP hidSEXP, SEXP IDswap_vecSEXP, SEXP IDswap_pool_vecSEXP, SEXP probSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::vector<int> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector<std::vector<int>> >::type similar(similarSEXP);
    Rcpp::traits::input_parameter< int >::type hid(hidSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type IDswap_vec(IDswap_vecSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type IDswap_pool_vec(IDswap_pool_vecSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type prob(probSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(test_sampleDonor_cpp(data, similar, hid, IDswap_vec, IDswap_pool_vec, prob, seed));
    return rcpp_result_gen;
END_RCPP
}
// test_unorderedSet
int test_unorderedSet(std::vector<int> x);
RcppExport SEXP _recordSwapping_test_unorderedSet(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(test_unorderedSet(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_recordSwapping_recordSwap_cpp", (DL_FUNC) &_recordSwapping_recordSwap_cpp, 10},
    {"_recordSwapping_setLevels_cpp", (DL_FUNC) &_recordSwapping_setLevels_cpp, 2},
    {"_recordSwapping_orderData_cpp", (DL_FUNC) &_recordSwapping_orderData_cpp, 2},
    {"_recordSwapping_test_randSample_cpp", (DL_FUNC) &_recordSwapping_test_randSample_cpp, 5},
    {"_recordSwapping_setRisk_cpp", (DL_FUNC) &_recordSwapping_setRisk_cpp, 4},
    {"_recordSwapping_test_distributeDraws_cpp", (DL_FUNC) &_recordSwapping_test_distributeDraws_cpp, 5},
    {"_recordSwapping_test_sampleDonor_cpp", (DL_FUNC) &_recordSwapping_test_sampleDonor_cpp, 7},
    {"_recordSwapping_test_unorderedSet", (DL_FUNC) &_recordSwapping_test_unorderedSet, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_recordSwapping(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
