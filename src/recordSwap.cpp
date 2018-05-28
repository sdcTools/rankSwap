#include <Rcpp.h>  // needed for using this in R
#include <iostream>     
#include <algorithm>    // std::count
#include <vector>       // std::vector
#include <array>

// Enable C++11 via this plugin (Rcpp 0.10.3 or later) - used for R only
// [[Rcpp::plugins(cpp11)]] 

using namespace std;
using namespace Rcpp; // needed for using this in R

/*
 * Function to reorder data-set given one index vector 
 */
// [[Rcpp::export]]
std::vector< std::vector<int> > orderData(std::vector< std::vector<int> > data, int orderIndex){
  
  // initialise ordering vector
  std::vector<int> orderVec(data[0].size());
  std::iota(orderVec.begin(),orderVec.end(),0);
  
  // order this vector by order of data[orderIndex]
  std::sort(orderVec.begin(),orderVec.end(),
    [&](int a, int b) { return data[orderIndex][a] < data[orderIndex][b]; }
  );

  // reorder data without copying it
  for(int i=0;i<orderVec.size();i++){
    // while orderVec[i] is not yet in place 
    // every swap places at least one element in it's proper place
    while(orderVec[i] !=   orderVec[orderVec[i]] ){
      // swap every "column" of data
      for(int j=0;j<data.size();j++){
        swap( data[j][orderVec[i]], data[j][orderVec[orderVec[i]]] );
      }
      // then adjust orderVec[i]
      swap( orderVec[i], orderVec[orderVec[i]] );
    }
  }
  
  return(data);
}


/*
* Function to define levels 
*/
// [[Rcpp::export]]
std::vector<int> setLevels(std::vector< std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> similar, std::vector<int> risk, int hid, int th) {
  
  // data: data input
  // hierarchy: column indices in data corresponding to geo hierarchy of data read left to right (left highest level - right lowest level)
  // similar: column indices in data corresponding to similarity variables which will be considered for swapping similar households
  // risk: column indices in data corresponding to risk variables which will be considered for estimating counts in the population
  // hid: int correspondig to column index in data which holds the household ID
  // th: int defining a threshold, each group with counts lower than the threshold will automatically be swapped.
  
  
  // initialise parameters
  int p = data.size();  // number of columns in data
  int n = data[0].size();  // number of rows in data
  
  int nsim = similar.size();
  int nhier = hierarchy.size();
  
  ////////////////////////////////////////////////////
  // order data by hid 
  data = orderData(data,hid);
  ////////////////////////////////////////////////////
  
  // initialise map
  std::map<std::vector<int>,int> group_count; 

  std::vector<int> loop_index=hierarchy;
  loop_index.insert(loop_index.end(),similar.begin(),similar.end());
  int loop_n = loop_index.size();
  // initialise vector for groups
  std::vector<int> groups(loop_n);
  // initialise map for household size
  std::map<int,int> map_hsize;
  
  ////////////////////////////////////////////////////
  // loop over data and ...
  for(int i=0;i<n;i++){
    
    // ... define group
    for(int j=0;j<loop_n;j++){
      groups[j] = data[loop_index[j]][i];
    }

    // ... count number for each group using std::map
    group_count[groups]++;
    
    // ... get household size map
    map_hsize[data[hid][i]]++;
  }
  ////////////////////////////////////////////////////

  ////////////////////////////////////////////////////
  // iterate over level_count and 
  // set level number for group if Number of obs in group <= th
  ////////////////////////////////////////////////////
  // initialise map with levels for each group
  std::map<std::vector<int>,int> level_number = group_count; 

  for(int i = 0;i<3;i++){
    // int i=2;  
    std::map<std::vector<int>,int> group_count_help = group_count;
    if(i>0){
      std::map<std::vector<int>,int> group_higher_help;
      
      // count occurences for higher grouping
      for(auto const&x : group_count_help){
        std::vector<int> groups_help = x.first;
        groups_help.resize(loop_n-i);
        group_higher_help[groups_help] += x.second; 
      }
      // loop over group_count_help again
      // and set the values to the map values in group_higher_help
      // those are the aggregates values
      for(auto const&x : group_count_help){
        std::vector<int> groups_help = x.first;
        groups_help.resize(loop_n-i);
        group_count_help[x.first] = group_higher_help[groups_help];
      }
    }
    
    for(auto const& x : group_count_help){ // -> loop over map entries
      if(x.second <= th){ // -> x.second = number of obs
        level_number[x.first] = nhier-i-1;
      }else{
        if(i==0){
          level_number[x.first] = nhier; //initialise with lowest level only at first loop
        }
      }
    }
  }
  ////////////////////////////////////////////////////
  
  //////////////////////////////////////////////////// 
  // initialise return vector for levels
  std::vector<int> data_level(n);
  int hsize=0;
  
  int i =0;
  int min_level=nhier;
  while(i<n){
    
    // get hsize of record i
    hsize = map_hsize[data[hid][i]];
    for(int h=0;h<hsize;h++){
      // ... define group
      for(int j=0;j<loop_n;j++){
        groups[j] = data[loop_index[j]][i+h];
      }
      min_level = std::min(min_level,level_number[groups]);
    }
    // apply minimum level to all data_level per hid
    for(int h=0;h<hsize;h++){
      data_level[i+h] = min_level;
    }
    i = i+hsize-1;
    min_level=nhier; // set min_level to nhier for each hid
  }
  ////////////////////////////////////////////////////

  return data_level;
}

