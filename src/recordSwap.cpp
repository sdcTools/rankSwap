#include <Rcpp.h>  // needed for using this in R
#include <iostream>     
#include <algorithm>    // std::count
#include <vector>       // std::vector
#include <random>
#include <queue>
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
std::vector<int> setLevels(std::vector< std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk, int hid, int th) {
  
  // data: data input
  // hierarchy: column indices in data corresponding to geo hierarchy of data read left to right (left highest level - right lowest level)
  // risk: column indices in data corresponding to risk variables which will be considered for estimating counts in the population
  // hid: int correspondig to column index in data which holds the household ID
  // th: int defining a threshold, each group with counts lower than the threshold will automatically be swapped.
  
  
  // initialise parameters
  int p = data.size();  // number of columns in data
  int n = data[0].size();  // number of rows in data
  
  int nhier = hierarchy.size();
  
  // initialise map
  std::map<std::vector<int>,int> group_count; 
  
  std::vector<int> loop_index=risk;
  loop_index.insert(loop_index.end(),hierarchy.begin(),hierarchy.end());
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
  

  for(int i = 0;i<nhier;i++){
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
    i = i+hsize;
    min_level=nhier; // set min_level to nhier for each hid
  }

  ////////////////////////////////////////////////////
  return data_level;
}

/*
 * Testfunction for performance
 */
// [[Rcpp::export]]
int test(std::vector< std::vector<int> > data, std::vector<int> loop_index) {
  
  // initialise parameters
  int n = data.size();  // number of columns in data
  int p = data[0].size();  // number of rows in data
  
  int loop_n = loop_index.size();
  
  
  /*
  std::vector<std::vector<int>> data_new(n,std::vector<int>(p));
  for(int i=0;i<loop_n;i++){
    for(int j=0;j<n;j++){
      data_new[j][loop_index[i]] = data[loop_index[i]][j];
    }
  }
  
  // initialise map
  std::map<std::vector<int>,int> group_count; 
  
  
  // initialise vector for groups
  // initialise map for household size
  std::map<int,int> map_hsize;
 
  ////////////////////////////////////////////////////
  // loop over data and ...
  for(int i=0;i<n;i++){
    
    // ... count number for each group using std::map
    group_count[data_new[i]]++;
    
  }
   */


  // initialise map
  std::map<std::vector<int>,int> group_count; 

  // initialise vector for groups
  std::vector<int> groups(loop_n);
  // initialise map for household size
  std::map<int,int> map_hsize;
  
  ////////////////////////////////////////////////////
  // loop over data and ...
  for(int i=0;i<n;i++){
    
    
    if(i==0){
      // ... define group
      for(int j=0;j<loop_n;j++){
        groups[j] = data[loop_index[j]][i];
      }
    }

    
    // ... count number for each group using std::map
    group_count[groups]++;

  }

  return 1;
}

/*
* Function to set sampling probability 
* and reverse sampling probability (for donor sets)
*/
// [[Rcpp::export]]
std::vector< std::vector<double> > setRisk(std::vector<std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk, int hid){
  
  // data: data input
  // hierarchy: column indices in data corresponding to geo hierarchy of data read left to right (left highest level - right lowest level)
  // risk: column indices in data corresponding to risk variables which will be considered for estimating counts in the population
  // hid: int correspondig to column index in data which holds the household ID
  
  // initialise parameters
  int n = data[0].size();
  int nhier = hierarchy.size();
  double risk_value=0;
  int current_ID;
  int hsize=0;
  // initialise probability data
  // index 0 corresponds to sampling probability for swapping
  // index 1 corresponds to sampling probability for donor set
  std::vector< std::vector<double> > prob(2, vector<double>(n));
  
  //
  std::vector<int> loop_index = risk;
  loop_index.insert(loop_index.end(),hierarchy.begin(),hierarchy.end());
  int loop_n = loop_index.size();
  std::vector<int> groups(loop_n);
  
  // initialise counts for groups in lowest hierarchy
  std::map<std::vector<int>,int> group_count; 
  for(int i=0;i<n;i++){
    
    // ... define group
    for(int j=0;j<loop_n;j++){
      groups[j] = data[loop_index[j]][i];
    }
    
    // ... count number for each group using std::map
    group_count[groups]++;
  }
  
  // loop again over data
  int i=0;
  int h=0;
  while(i<n){
    
    current_ID = data[hid][i];
    while(current_ID==data[hid][i+h]){
      // ... define group
      for(int l=0;l<loop_n;l++){
        groups[l] = data[loop_index[l]][i+h];
      }
      
      // select highest risk for hid
      risk_value = max(risk_value,1.0/group_count[groups]);
      hsize++;
      h++;
    }
    // assign highst risk and revers risk to all household members
    for(int j=0;j<hsize;j++){
      prob[0][i+j] = risk_value;
      if(risk_value<1){
        prob[1][i+j] = 1.0 - risk_value;
      }else{
        prob[1][i+j] = 5e-10;
      }
    }
    
    // reset parameters
    i = i+hsize;
    hsize=0;
    risk_value = 0;
    h =0;
  }
  
  return prob;
}



/*
* Function to sample from std::vector<int> given a probability vector
*/
std::vector<int> randSample(std::vector<int> &ID, int N, std::vector<double> &prob, std::mt19937 &mersenne_engine,
                            std::vector<int> &IDused, std::unordered_set<int> &mustSwap){
  
  // initialise parameters
  std::exponential_distribution<double> exp_dist(1.0); // initialise lambda para for exp distribution
  /*
  * from R-Package ‘wrswoR’:
  * We need the last "size" elements of
  * U ^ (1 / prob) ~ log(U) / prob
  * ~ -Exp(1) / prob
  * ~ prob / Exp(1)
  * Here, ~ means "doesn't change order statistics".
  */  
  
  // generate random numbers -> prob[i]/exp_dist(mersenne_engine)
  // and store them in priority queue
  // get index of N largest elements in randVal
  // from https://stackoverflow.com/questions/14902876/indices-of-the-k-largest-elements-in-an-unsorted-length-n-array
  // use priority_queue
  std::priority_queue<std::pair<double, int>> q;
  std::unordered_set<int> select_mustSwap;

  for(int i=0;i<ID.size();i++){
    if(IDused[ID[i]]==0){
      if(mustSwap.find(ID[i])!=mustSwap.end()){
        select_mustSwap.insert(ID[i]);
      }else{
        q.push(std::pair<double, int>(prob[ID[i]]/exp_dist(mersenne_engine), i));
      }
    }
  }

  
  N = min<int>(q.size(),max<int>(N,select_mustSwap.size()));
  std::vector<int> sampleID(N);
  // select index of top elements from priority_queue
  int j=0;
  for(auto s: select_mustSwap){
    if(j<N){
      sampleID[j] = s;
      j++;
    }else{
     break; 
    }
  }
  for(int i=j;i<N;i++){
    sampleID[i] = ID[q.top().second]; //.top() access top element in queue
    q.pop(); // remove top element in queue
  }
  
  return sampleID;
}

/*
* test random sampling and seed
* used to check of state of RNG changes successfully with function calls
*/
// [[Rcpp::export]]
std::vector<std::vector<int>> test_randSample(int B,std::vector<int> ID, int N, std::vector<double> prob,int seed){
  
  std::vector<std::vector<int>> output(B,std::vector<int>(N));
  // set random seed according to input parameter and
  // initialize random number generator with seed
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  
  std::vector<int> IDused(ID.size(),0);
  std::unordered_set<int> mustSwap;
  std::unordered_set<int> mustSkip;
  
  int i=0;
  while(i<B){
    output[i] = randSample(ID,N,prob,mersenne_engine,IDused,mustSwap);
    ++i;
  }
  
  return output;
}

// [[Rcpp::export]]
std::vector<int> test_stuff(std::vector<int> vec1){
  
  std::unordered_set<int> mymap(vec1.begin(),vec1.end());
  std::vector<int> vec2(vec1.size());
  
  for(int i=0;i<vec1.size();i++){
    if(mymap.find(vec1[i])!=mymap.end()&&vec1[i]!=4){
      vec2[i] = vec1[i];
    }
  }
  return vec2;
}



/*
* Function to perform record swapping
*/
// [[Rcpp::export]]
std::vector<std::vector<int>> recordSwap(std::vector< std::vector<int> > data, std::vector<int> similar,
                                         std::vector<int> hierarchy, std::vector<int> risk, int hid, int th, double swaprate,
                                         std::vector<std::vector<double>> prob, std::vector<int> levels, int seed = 123456){
  
  // data: data input
  // hierarchy: column indices in data corresponding to geo hierarchy of data read left to right (left highest level - right lowest level)
  // similar: column indices in data corresponding to variables (household/personal) which should be considered when swapping,
  // e.g. swapping onlys household with same houshoeld size 
  // risk: column indices in data corresponding to risk variables which will be considered for estimating counts in the population
  // hid: int correspondig to column index in data which holds the household ID
  // th: int defining a threshold, each group with counts lower than the threshold will automatically be swapped.
  // swaprate: double defining the ratio of households to be swapped
  // seed: integer seed for random number generator
  
  
  // initialise parameters
  int n = data[0].size();
  int nhier = hierarchy.size();
  // needed for running random number generator and
  // set random seed according to input parameter
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  
  // std::vector<int> levels(n);
  // std::vector< std::vector<double> > prob(2, vector<double>(n));
  
  ////////////////////////////////////////////////////
  // order data by hid 
  // data = orderData(data,hid);
  ////////////////////////////////////////////////////
  
  ////////////////////////////////////////////////////
  // define minimum swap level for each household
  // levels = setLevels(data,hierarchy,risk,hid,th);
  ////////////////////////////////////////////////////
  
  ////////////////////////////////////////////////////
  // define sampling probabilities
  // prob = setRisk(data, hierarchy, risk, hid);
  ////////////////////////////////////////////////////
  
  ////////////////////////////////////////////////////
  // define number of swaps on each level (discard this step????)
  
  
  ////////////////////////////////////////////////////
  // this part will be moved further up after testing
  // initialise map for household size
  std::unordered_map<int,int> map_hsize;
  // loop over data and ...
  for(int i=0;i<n;i++){
    // ... get household size map
    map_hsize[data[hid][i]]++;
  }
  ////////////////////////////////////////////////////
  
  ////////////////////////////////////////////////////
  // apply swapping algorithm
  // go from highest to lowest level
  // swapp at each higher level the number of households that have to be swapped at that level according to "th" (see setLevels())
  // at lowest level swap remaining number of households (according to swap) if not enough households have been swapped
  // every household can only be swapped once 

  std::map<std::vector<int>,std::vector<int>> group_hier; //
  std::unordered_map<int,std::unordered_set<int>> group_levels; // map containing all IDs which must be swapped at a certain level (~key of map)
  std::vector<int> hier_help(nhier); // help vector to get hierarchy groups
  std::unordered_map<int,int> swappedIndex; // map for indices that have already been used -> unorderd map constant time access
  std::vector<int> IDused(n,0); // 0-1 vector if 1 this index was already swapped and cant be swapped again
  int z=0; // counter used for while() ect...
  int z2=0; // second counter used for lowest hierarchy
  std::vector<int>::iterator IDpos; // iterator used in loops
  int n_swapped_before = 0; // count how many IDs have been swapped in previous iterations (regarding hierarchy)
  int n_swapped = 0; // number of already swapped units 
  // create map containing subgroups according to hierarchy
  // and IDs of each subgroup
  // use hhsize for this to speed things up
  while(z<n){
    
    // ... define hierarchy group
    for(int j=0;j<nhier;j++){
      hier_help[j] = data[hierarchy[j]][z];
    }
    
    // supply new household index to each group
    // use only indices to speed up construction of output data
    group_hier[hier_help].push_back(z);
    
    // create map for levels
    if(levels[z]<nhier){
      group_levels[levels[z]].insert(z);
    }
    
    // skip all other household member, only need first one
    z += map_hsize[data[hid][z]];
  }
  
  // get number of households at lowest level hierarchy
  // this is only used at lowest hierarchy level
  std::map<std::vector<int>,int> n_group;
  for(auto const&x : group_hier){
    n_group[x.first]=x.second.size();
  }
  
  // Procedure for swapping starts here:
  // loop over hierarchies 
  // start at highest hierarchy
  
  for(int h=0;h<nhier;h++){
  // int h=3;
    // values of map element that must be swapped at current stage
    // if no elements need to be swapped than skipp this step
    std::unordered_set<int> mustSwap;
    if(group_levels.find(h)!=group_levels.end()){
      mustSwap = group_levels[h];
    }else{
      if(h<nhier-1){
        // skip loop iteration if no items need to be swapped at this stage
        // do not skip if at lowest stage
        continue;
      }
    }
    
    std::map<std::vector<int>,std::vector<int>> group_hier_help;
    hier_help.resize(h+1);
    
    /////////////////
    // get combined map for hierarchy h
    for(auto const&x : group_hier){
      // discard every index that has already been used
      // more efficient to do this at this step then later on in the code
      std::vector<int> IDnotused(x.second.size());
      z=0;
      for(int i=0;i<x.second.size();i++){
        if(IDused[IDnotused[z]]==0){
          IDnotused[z] = x.second[i];
          ++z;
        }
      }
      // get higher hierarchy
      for(int i=0;i<h+1;i++){
        hier_help[i] = x.first[i];
      }
      // append ID vector for each map element
      group_hier_help[hier_help].insert(group_hier_help[hier_help].end(),IDnotused.begin(),IDnotused.begin()+z); 

    }
    /////////////////

    
    /////////////////
    // create vector of IDs sorted by hierarchy levels (unordered map)
    std::vector<int> IDdonor_all;
    std::vector<int> n_IDdonor(group_hier_help.size()+1);
    n_IDdonor[0]=0;
    z=1;
    for(auto const&x : group_hier_help){
      
      // append ID vector for each map element
      IDdonor_all.insert(IDdonor_all.end(),x.second.begin(),x.second.end());
      // get number of elements for each group
      n_IDdonor[z]=n_IDdonor[z-1]+x.second.size();
      ++z;
    }
    /////////////////
    int sampSize=0;
    int k=1;
    int countUsed=0;
    int countRest=0;

    /////////////////
    // loop over levels of hierarchy
    for(auto const&x : group_hier_help){
      // std::vector<int> xfirst = group_hier_help.begin()->first;
      // std::vector<int> xsecond = group_hier_help.begin()->second;
      
      // get values that need to be swapped at this hierarchy level and which are
      // in this hierarchy stage
      // loop over values in x.second
      std::vector<int> IDswap(x.second.size());

      if(h<nhier-1){
        z =0;
        if(mustSwap.size()){
          for(int i=0;i<x.second.size();i++){
            // if there are any households that must be swapped due to variable th
            if(IDused[x.second[i]]==0 && mustSwap.find(x.second[i])!=mustSwap.end()){
              IDswap[z]=x.second[i];
              z++;
            }
          }
        }
        IDswap.resize(z);
      }else{
        // if at lowest level get number of households that need to be swapped
        // according to swap and check if this number was already reached
        // by previous swappings
   
        countUsed = n_group[x.first] - x.second.size();
        
        // not enough households have been swapped
        // when checking at lowest level
        // Number of IDs that need to be swappe - already swapped IDs - IDs that have to be swapped at lowest level:
        countRest = ceil(n_group[x.first]*swaprate) - countUsed;
        if(countRest>0){
          /*
          // some IDs must still be swapped
          // draw using prob from risk
          std::vector<int> IDswap_draw(x.second.size());
          // remove all values from IDswap_draw which are already in IDswap
          // or have already been swapped before
          z=0;
          z2=0;
          if(mustSwap.size()>0){
            for(int i=0;i<IDswap_draw.size();i++){
              if(IDused[x.second[i]]==0){
                if(mustSwap.find(x.second[i])!=mustSwap.end()){
                  IDswap[z]=x.second[i];
                  z++;
                }else{
                  IDswap_draw[z2] = x.second[i];
                  z2++;
                }
              }
            }
          }else{
            for(int i=0;i<IDswap_draw.size();i++){
              if(IDused[x.second[i]]==0){
                IDswap_draw[z2] = x.second[i];
                z2++;
              }
            }
          }
          IDswap_draw.resize(z2);
          IDswap.resize(z);
          
           // draw from IDswap_draw vector
          // if IDswap_draw contains any IDs
          // if this vector is empty then all IDs have already been swapped
          if(IDswap_draw.size()>0){
            // create probability vector
            std::vector<double> prob_draw(IDswap_draw.size(),0.0);
            std::transform(x.second.begin(),x.second.end(),prob_draw.begin(),[prob](size_t pos){return prob[0][pos];});
            std::vector<int> IDswap_add = randSample(IDswap_draw,countRest,prob_draw,mersenne_engine,IDused,mustSwap);
            // append vector to IDwap
            IDswap.insert(IDswap.end(),IDswap_add.begin(),IDswap_add.end());
          }
           */
          std::vector<int> IDswap_draw = x.second;
          std::unordered_set<int> mustSkip1;
          std::vector<int> IDswap_add = randSample(IDswap_draw,countRest,prob[0],mersenne_engine,IDused,mustSwap);
          for(int i=0;i<IDswap_add.size();i++){
            IDswap[i] = IDswap_add[i];
          }
          IDswap.resize(IDswap_add.size());
        }
      }

      
      // if any IDs need to be swapped:
      if(IDswap.size()>0){
        // get donor IDs
        // remove k-th group from IDdonor
        /*
        std::vector<int> IDdonor(IDdonor_all.size());

        // use only IDs which have not been used already and skip 
        z=0;
        for(int i=0;i<IDdonor_all.size();i++){
          if(IDused[IDdonor_all[i]]==0 && (i<n_IDdonor[k-1] || i>=n_IDdonor[k])){
            IDdonor[z] = IDdonor_all[i];
            z++;
          }
        }
        IDdonor.resize(z);
         */
        // THIS NEEDS TO BE FIXED ??? NOT ENOUGH DONORS CAN ACTUALLY HAPPEN!?!?!?!?
        //
        //if(IDdonor.size()>0){
          // sample IDswap.size() elements from IDdonor
          // create sampling probability vector
          // std::vector<double> prob_donor(IDdonor.size(),0.0);
          // std::transform(IDdonor.begin(),IDdonor.end(),prob_donor.begin(),[prob](size_t pos){return prob[1][pos];});
          
          // number of swaps for donor set
          // take minimum of number of swaps from IDswap and IDdonor.size()
          //sampSize = min(IDswap.size(),IDdonor.size());
          sampSize = IDswap.size();
          std::vector<int> sampledID = randSample(IDdonor_all,sampSize,prob[1],mersenne_engine,IDused,mustSwap);
          
          // set Index to used
          for(int i=0;i<sampSize;i++){
            IDused[sampledID[i]]=1;
            IDused[IDswap[i]]=1;
            // store results from sampling in swappedIndex and swappedIndexwith
            swappedIndex[IDswap[i]] = sampledID[i];
          }
        //}
      }
      // increment k by one
      // is needed for discarding subset of donor data
      ++k;
      /////////////////
    }
  }

  // resize swappedIndex and swappedIndexwith
  // will have trailin zeros which are not needed
/*
  ////////////////////////////////////////////////////
  // Create output using swappedIndex and swappedIndexwith
  std::vector<int> swap_hierarchy(nhier);
  int hsize=0;
  int hsizewith=0;
  for(auto const&x : swappedIndex){
    hsize = map_hsize[data[hid][x.first]];
    hsizewith= map_hsize[data[hid][x.second]];
    // loop over hierarchy
    for(int j=0;j<nhier;j++){
      swap_hierarchy[j] = data[hierarchy[j]][x.first];
      
      for(int h=0;h<max(hsize,hsizewith);h++){
        // swap hierarchy for every household member in x.first
        if(h<hsize){
          data[hierarchy[j]][x.first+h] = data[hierarchy[j]][x.second];
        }
        // swap hierarchy for every household member in x.second
        if(h<hsizewith){
          data[hierarchy[j]][x.second+h] = swap_hierarchy[j];
        }
      }
    }
  }
 */
  // contruct dummy output
  std::vector<std::vector<int>> out(2,std::vector<int>(swappedIndex.size()));
  z=0;
  for(auto const&x : swappedIndex){
    out[0][z] = x.first;
    out[1][z] = x.second;
    z++;
  }

  ////////////////////////////////////////////////////
  // Define output
  return out;


}


