#include <Rcpp.h>
using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later) - used for R only
// [[Rcpp::plugins(cpp11)]] 

#include "recordSwap/recordSwap.cpp"


//' @title Targeted Record Swapping
//' 
//' @description Applies targeted record swapping on micro data considering the identification risk of each record as well the geographic topology.
//'  
//' @details The procedure accepts a rectanglar data set containing all necessary information for the record swapping, e.g parameter \code{hid}, \code{similar}, \code{hierarchy}, ect... \cr 
//' First the micro data in \code{data} is ordered by \code{hid} and the identification risk is calculated for each record. As of right now only counts is used as 
//' identification risk and the inverse of counts is used as sampling probability. \cr
//' \cr 
//' In addition each record gets an index corresponding to the hierarchy level over which the household must be swapped. For instance having a geographic hierarchy
//' of county > municipality > district the risk is calculated for each geographic variable and defined risk variables. If a record falls below \code{th} for hierachy county then this record
//' needs to be swapped accross counties. \cr
//' \cr 
//' After that the targeted record swapping is applied starting from the highest to the lowest hierarchy level and cycling through all possible geographic areas at each hierarchy level, e.g every county, every municipality in ever county, ect...
//' At each geographic area a set of values is created for records to be swapped.\cr
//' In all but the lowest hierarchy level this is made out of all records which do not fullfill the k-anonymity and have not already been swapped.
//' Those records are swapped with records not belonging to the same geographic area, which have not already been swapped before hand. Swapping referes to the interchange of geographic variables defined in \code{hierarchy}.
//' When a record is swapped all other record containing the same \code{hid} are swapped as well. \cr
//' \cr 
//' At the lowest hierarchy level in every geographic area the set of records to be swapped is made up of all records which do not fullfill the k-anonymity as well as the remaining numer of records such
//' that the proportion of swapped records of the geographic area is equal to \code{swaprate}. k-anonymity is implemented in the sense that a household will be swapped if \code{counts<=th}. If, due to the k-anonymity condition, more records have already been swapped in this geographic area then only the 
//' records which do not fullfill the k-anonymity are swapped.
//' 
//' @param data micro data set containing only integer values.
//' @param similar vector of vector where each entry corresponds to column indices of variables in \code{data} which should be considered when swapping households, see details for more explanations
//' @param hierarchy column indices of variables in \code{data} which refere to the geographic hierarchy in the micro data set. For instance county > municipality > district.
//' @param risk_variables column indices of variables in \code{data} which will be considered for estimating the risk.
//' @param hid column index in \code{data} which refers to the household identifier.
//' @param k_anonymity integer defining the threshhold of high risk households (k-anonymity). This is used as k_anonymity <= counts.
//' @param swaprate double between 0 and 1 defining the proportion of households which should be swapped, see details for more explanations
//' @param risk_threshold double indicating risk threshold above every household needs to be swapped.
//' @param risk vector of vectors containing risks of each individual in each hierarchy level.
//' @param seed integer defining the seed for the random number generator, for reproducability.
//' 
//' @return Returns data set with swapped records.
// [[Rcpp::export]]
std::vector< std::vector<int> > recordSwap_cpp(std::vector< std::vector<int> > data, std::vector<std::vector<int>> similar,
                                               std::vector<int> hierarchy, std::vector<int> risk_variables, int hid, int k_anonymity, double swaprate,
                                               double risk_threshold, std::vector<std::vector<double>> risk,
                                               int seed = 123456){
  //std::vector<std::vector<int> > output = recordSwap(data,similar,hierarchy,risk,hid,th,swaprate,seed);
  std::vector< std::vector<int> > output = recordSwap(data,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate,risk_threshold,risk,seed);
  return output;
}

//' @title Define Swap-Levels
//' 
//' @description Define hierarchy levels over which record needs to be swapped according to risk variables.
//' 
//' 
//' @param risk vector of vectors containing risks of each individual in each hierarchy level. risk[0] returns the vector of risks for the first unit over all hierarchy levels.
//' risk[1] the vectir if risks for all hierarchy level of unit 2, and so on.
//' @param risk_threshold double defining the risk threshold beyond which a record/household needs to be swapped. This is understood as risk>=risk_threshhold.
//' 
//' @return Integer vector with hierarchy level over which record needs to be swapped with.
// [[Rcpp::export]]
std::vector<int> setLevels_cpp(std::vector< std::vector<double> > risk, double risk_threshold) {
  std::vector<int> output = setLevels(risk, risk_threshold);
  return output;
}

//' @title Reorder data
//' 
//' @description Reorders the data according to a column in the data set. This procedure is used inside \code{\link{recordSwap_cpp}} to speed up performance.
//' 
//' @param data micro data set containing only numeric values.
//' @param orderIndex column index in \code{data} refering to the column by which data should be ordered.
//' 
//' @return ordered data set.
// [[Rcpp::export]]
std::vector< std::vector<int> > orderData_cpp(std::vector< std::vector<int> > &data, int orderIndex){
  std::vector< std::vector<int> > output = orderData(data,orderIndex) ;
 return output ;
}

// [[Rcpp::export]]
std::vector<int> test_randSample_cpp(std::vector<int> ID, int N, std::vector<double> prob, std::vector<int> IDused, int seed){
  std::vector<int> output = test_randSample(ID,N,prob,IDused,seed);
  return output;
}

//' @title Calculate Risk
//' 
//' @description Calculate risk for records to be swapped and donor records.  Risks are defined by 1/counts, where counts is the number of records with the same values for specified risk variables in the each geographic hierarchy.
//' This risk will be used as sampling probability for both sampling set and donor set.
//' 
//' @param data micro data set containing only numeric values.
//' @param hierarchy column indices of variables in \code{data} which refere to the geographic hierarchy in the micro data set. For instance county > municipality > district.
//' @param risk_variables column indices of variables in \code{data} which will be considered for estimating the risk.
//' @param hid column index in \code{data} which refers to the household identifier.
//' 
// [[Rcpp::export]]
std::vector< std::vector<double> > setRisk_cpp(std::vector<std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk_variables, int hid){
  std::vector< std::vector<double> > output = setRisk(data,hierarchy,risk_variables,hid);
  return output;
}


// [[Rcpp::export]]
std::vector<std::vector<int>> test_distributeDraws_cpp(std::vector< std::vector<int> > data,
                                                                   std::vector<int> hierarchy,  int hid, double swaprate, int seed = 123456){
  
  std::vector<std::vector<int>> output = test_distributeDraws(data,hierarchy,hid,swaprate,seed);
  return output;
}


// [[Rcpp::export]]
std::vector<int> test_sampleDonor_cpp(std::vector< std::vector<int> > data, std::vector<std::vector<int>> similar, int hid,
                                                   std::vector<int> IDswap_vec, std::vector<int> IDswap_pool_vec, std::vector<double> prob, int seed=123456){
  
  std::vector<int> output = test_sampleDonor(data, similar, hid, IDswap_vec, IDswap_pool_vec, prob, seed);
  return output;
}

// [[Rcpp::export]]
std::vector<int> test_prioqueue(std::vector<int> x_vec,std::vector<double> prob,std::vector<int> mustSwap_vec,int n,int seed){

  std::unordered_set<int> mustSwap(mustSwap_vec.begin(),mustSwap_vec.end());
  std::unordered_set<int> x(x_vec.begin(),x_vec.end());
  
  std::priority_queue<std::pair<double, int> > q;
  std::vector<int> sampleID(x.size());
  std::exponential_distribution<double> exp_dist(1.0);
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);

  int z=0;
  for(auto s : x){
    if(mustSwap.find(s)!=mustSwap.end()){
      sampleID[z] = s;
      z++;
    }else{
      q.push(std::pair<double, int>(prob[s]/exp_dist(mersenne_engine), s));
    }
  }
  
  n = max(0,min<int>(q.size(),n-z));
  sampleID.resize(n+z);

  // select index of top elements from priority_queue
  for(int i=0;i<n;i++){
    sampleID[i+z] = q.top().second; //.top() access top element in queue
    q.pop(); // remove top element in queue
  }

  return sampleID;
}



struct Comp{
  Comp(const std::vector<double>& v ) : _v(v) {}
  // Inverted comparison!
  bool operator ()(int a, int b) { return _v[a] > _v[b]; }
  const std::vector<double>& _v;
};

// [[Rcpp::export]]
std::vector<int> test_comparator(std::vector<int> x_vec,std::vector<double> prob,std::vector<int> mustSwap_vec, int n, int seed){
  
  std::unordered_set<int> mustSwap(mustSwap_vec.begin(),mustSwap_vec.end());
  std::unordered_set<int> x(x_vec.begin(),x_vec.end());
  
  std::exponential_distribution<double> exp_dist(1.0);
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  int N = x.size();

  // apply prob[i]/exp_dist(mersenne_engine) over whole prob vector
  std::for_each(prob.begin(),prob.end(), [&exp_dist, &mersenne_engine](double& d) {d = d/exp_dist(mersenne_engine);});
  // define highest sampling value and give all elements in mustSwap this probabilits (they will always be selected)
  std::vector<int> x_return(x.begin(),x.end());
  int z=0;
  for(int i=0;i<N;i++){
    if(mustSwap.find(x_return[i])!=mustSwap.end()){
      iter_swap(x_return.begin() + z, x_return.begin() + i);
      z++;
    }
  }

  // sort x by prob
  std::partial_sort(x_return.begin()+z,x_return.begin()+n-z,x_return.end(),Comp(prob));
  x_return.resize(n);
  
  return x_return;
}
