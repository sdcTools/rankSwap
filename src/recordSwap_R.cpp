#include <Rcpp.h>
using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later) - used for R only
// [[Rcpp::plugins(cpp11)]] 

#include "recordSwap/recordSwap.cpp"

// [[Rcpp::export]]
std::vector<std::vector<int> > recordSwap_cpp(std::vector< std::vector<int> > data, std::vector<int> similar,
                                          std::vector<int> hierarchy, std::vector<int> risk, int hid, int th, double swaprate,
                                          int seed = 123456){
  std::vector<std::vector<int> > output = recordSwap(data,similar,hierarchy,risk,hid,th,swaprate,seed);
  return output;
}

// [[Rcpp::export]]
std::vector<int> setLevels_cpp(std::vector< std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk, int hid, int th) {
  std::vector<int> output = setLevels(data,hierarchy,risk,hid,th);
  return output;
}
// [[Rcpp::export]]
std::vector< std::vector<int> > orderData_cpp(std::vector< std::vector<int> > &data, int orderIndex){
  std::vector< std::vector<int> > output = orderData(data,orderIndex) ;
 return output ;
}

// [[Rcpp::export]]
std::vector<std::vector<int> > test_randSample_cpp(int B,std::vector<int> ID, int N, std::vector<double> prob,int seed){
  std::vector<std::vector<int> > output = test_randSample(B,ID,N,prob,seed);
  return output;
}

// [[Rcpp::export]]
std::vector< std::vector<double> > setRisk_cpp(std::vector<std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk, int hid){
  std::vector< std::vector<double> > output = setRisk(data,hierarchy,risk,hid);
  return output;
}
// 
// // [[Rcpp::export]]
// std::unordered_set<int> randSample_cpp(std::unordered_set<int> &ID, int N, std::vector<double> &prob, std::mt19937 &mersenne_engine,
//                                    std::vector<int> &IDused, std::unordered_set<int> &mustSwap){
//   std::unordered_set<int> output = randSample(ID,N,prob,mersenne_engine,IDused,mustSwap)
//   return output;
// }