#include <Rcpp.h>
using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later) - used for R only
// [[Rcpp::plugins(cpp11)]] 

#include "recordSwap/recordSwap.cpp"


//' @title Targeted Record Swapping
//' 
//' @description Applies targeted record swapping on micro data set, see \link{\code{recordSwap}} for details.
//' \cr
//' NOTS: This is an internal function called by the R-function `recordSwap()`. It's only purpose is to include the C++-function recordSwap() using Rcpp.
//' 
//' @param data micro data set containing only integer values. A data.frame or data.table from R needs to be transposed beforehand so that data.size() ~ number of records - data.[0].size ~ number of varaibles per record.
//' @param similar List where each entry corresponds to column indices of variables in \code{data} which should be considered when swapping households.
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
std::vector< std::vector<int> > recordSwap_cpp(std::vector< std::vector<int> > data, Rcpp::List similar_cpp,
                                               std::vector<int> hierarchy, std::vector<int> risk_variables, int hid, int k_anonymity, double swaprate,
                                               double risk_threshold, std::vector<std::vector<double>> risk,
                                               int seed = 123456){
  // prep inputs for the call to recordSwap()
  // some formats can not directly be transformed to stl-containers
  std::vector<std::vector<int>> similar(similar_cpp.size());
  for(int i=0;i<similar_cpp.size();i++){
    Rcpp::List sublist = similar_cpp[i];
    int m = sublist.size();
    for(int j=0;j<m;j++){
      similar[i].push_back(sublist[j]);
    }
  }
  
  // call recrodSwap()
  std::vector< std::vector<int> > output = recordSwap(data,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate,risk_threshold,risk,seed);
  return output;
}

//' @title Define Swap-Levels
//' 
//' @description Define hierarchy levels over which record needs to be swapped according to risk variables.
//' \cr
//' NOTE: This is an internal function used for testing the C++-function `setLevels()` which is applied inside `recordSwap()`.
//' 
//' 
//' @param risk vector of vectors containing risks of each individual in each hierarchy level. risk[0] returns the vector of risks for the first unit over all hierarchy levels.
//' risk[1] the vector if risks for all hierarchy level of unit 2, and so on.
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
//' @description Reorders the data according to a column in the data set.
//' \cr
//' NOTE: This is an internal function used for testing the C++-function `orderData` which is used inside the C++-function `recordSwap` to speed up performance.
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


//' @title Calculate Risk
//' 
//' @description Calculate risk for records to be swapped and donor records.  Risks are defined by 1/counts, where counts is the number of records with the same values for specified `risk_variables` in the each geographic hierarchy.
//' This risk will be used as sampling probability for both sampling set and donor set.
//' \cr
//' NOTE: This is an internal function used for testing the C++-function `setRisk` which is used inside the C++-function `recordSwap`.
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


//' @title Random Sampling
//' 
//' @description Randomly select records given a proabability weight vector `prob`. 
//' \cr
//' NOTE: This is an internal function used for testing the C++-function `randSample` which is used inside the C++-function `recordSwap`.
//' 
//' @param ID vector containing record IDs from which to sample
//' @param N integer defining the number of records to be sampled
//' @param prob a vector of probability weights for obtaining the elements of the vector being sampled.
//' @param IDused vector containing IDs which must not be sampled
//' @param seed integer setting the sampling seed
//' 
// [[Rcpp::export]]
std::vector<int> randSample_cpp(std::vector<int> ID, int N, std::vector<double> prob,std::vector<int> IDused, int seed){
  
  // set random seed according to input parameter and
  // initialize random number generator with seed
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  
  std::unordered_set<int> mustSwap;
  std::unordered_set<int> ID_set(ID.begin(),ID.end());
  
  std::vector<int> output = randSample(ID_set,N,prob,mersenne_engine,IDused,mustSwap);
  
  return output;
}


//' @title Distribute number of swaps
//' 
//' @description Distribute number of swaps across lowest hierarchy level according to a predefinde `swaprate`. The swaprate is applied such that a single swap counts as swapping 2 households.
//' Number of swaps are randomly rouded up or down, if needed, such that the total number of swaps is in coherence with the swaprate.
//' \cr
//' NOTE: This is an internal function used for testing the C++-function `distributeDraws` which is used inside the C++-function `recordSwap`.
//' 
//' @param data micro data containing the hierarchy levels and household ID
//' @param hierarchy column indices of variables in \code{data} which refere to the geographic hierarchy in the micro data set. For instance county > municipality > district.
//' @param hid column index in \code{data} which refers to the household identifier.
//' @param swaprate double between 0 and 1 defining the proportion of households which should be swapped, see details for more explanations
//' @param seed integer setting the sampling seed
//' 
// [[Rcpp::export]]
std::vector< std::vector<int> > distributeDraws_cpp(std::vector< std::vector<int> > data,
                                                     std::vector<int> hierarchy, int hid, double swaprate, int seed = 123456){
  
  // define parameter
  int n = data.size();
  int nhier = hierarchy.size();
  int nhid = 0;
  int currentID = -1;
  
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  // initialize random number generator for distributeDraws()
  std::uniform_int_distribution<std::mt19937::result_type> runif01(0,1);
  
  std::map<std::vector<int>,std::unordered_set<int> > group_hier; //
  std::vector<int> hier_help(nhier); // help vector to get hierarchy groups
  
  for(int i=0;i<n;i++){
    
    if(currentID==data[i][hid]){
      continue; // go to next iteration if statement is true
    }
    
    currentID = data[i][hid];
    // ... define hierarchy group
    for(int j=0;j<nhier;j++){
      hier_help[j] = data[i][hierarchy[j]];
    }
    
    // supply new household index to each group
    // use only indices to speed up construction of output data
    group_hier[hier_help].insert(i);
    
    // count number of households
    nhid++;
    // skip all other household member, only need first one
  }
  
  std::map<std::vector<int>,std::pair<int,int>> draw_group =  distributeDraws(group_hier, nhid, swaprate, 
                                                                              runif01, mersenne_engine);
  
  
  // iterate over map to generate output
  // implementation good enough for testing purposes
  std::vector<std::vector<int>> output(draw_group.size(),std::vector<int>(nhier+2));
  int z = 0;
  for(auto const&x : draw_group){
    for(int j=0;j<(nhier+2);j++){
      if(j<nhier){
        output[z][j] = x.first[j];
      }else if(j==nhier){
        output[z][j] = x.second.first;
      }else{
        output[z][j] = x.second.second;
      }
    }
    z++;
  }
  return output;
}




/*
 * Function to test sampleDonor
 */
//' @title Random sample for donor records
//' 
//' @description Randomly select donor records given a probability weight vector. This sampling procedure is implemented differently than \link{\code{randSample_cpp}} to speed up performance of C++-function `recordSwap`.
//' \cr
//' NOTE: This is an internal function used for testing the C++-function `sampleDonor` which is used inside the C++-function `recordSwap`.
//' 
//' @param data micro data containing the hierarchy levels and household ID
//' @param similar List where each entry corresponds to column indices of variables in \code{data} which should be considered when swapping households.
//' @param hid column index in \code{data} which refers to the household identifier.
//' @param IDswap vector containing records for which a donor needs to be sampled
//' @param IDswap_pool_vec set from which `IDswap` was drawn
//' @param prob a vector of probability weights for obtaining the elements of the vector being sampled.
//' @param seed integer setting the sampling seed
//' 
// [[Rcpp::export]]
std::vector<int> sampleDonor_cpp(std::vector< std::vector<int> > data, Rcpp::List similar_cpp, int hid,
                                  std::vector<int> IDswap, std::vector<int> IDswap_pool_vec, std::vector<double> prob, int seed=123456){
  
  
  // prep inputs for the call to sampleDonor()
  // some formats can not directly be transformed to stl-containers
  std::vector<std::vector<int>> similar(similar_cpp.size());
  for(int i=0;i<similar_cpp.size();i++){
    Rcpp::List sublist = similar_cpp[i];
    int m = sublist.size();
    for(int j=0;j<m;j++){
      similar[i].push_back(sublist[j]);
    }
  }
  
  // generate paramters
  int n = data.size();
  std::vector<int> IDused(n);
  std::unordered_set<int> IDswap_pool(IDswap_pool_vec.begin(),IDswap_pool_vec.end());
  
  // generate IDdonor_pool
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  // initialise lambda para for exp distribution
  std::exponential_distribution<double> exp_dist(1.0);
  std::map<double,int> IDdonor_pool;
  for(int i=0; i<n; i++){
    IDdonor_pool[prob[i]/exp_dist(mersenne_engine)] = i;
  }
  
  std::vector<int> IDdonor = sampleDonor(data, similar, IDswap, IDswap_pool,
                                         IDdonor_pool, IDused, hid);
  
  return IDdonor;
}


/*
 * Some test functions
 * NOT USED AS OF RIGHT NOW!
 */
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
