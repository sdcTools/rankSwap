#include <iostream>     
#include <algorithm>    // std::count
#include <vector>       // std::vector
#include <random>
#include <queue>
#include <array>
#include <map>
#include <unordered_set>
#include <unordered_map>
using namespace std;
#include "recordSwap.h"

/*
* Function to reorder data-set given one index vector 
*/
std::vector< std::vector<int> > orderData(std::vector< std::vector<int> > &data, int orderIndex){
  
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
 * this function returns the hierarchy level over which a unit/household needs to be swapped
 * 0 meaning the highest hierarchy level, 1 the second highest hierarchy level, and so on....
 */
std::vector<int> setLevels(std::vector< std::vector<double> > &risk, double risk_threshold) {
  
  // risk: data containing the risk for each hierarchy level and each unit. risk[0] returns the vector of risks for the first unit over all hierarchy levels
  // risk_threshold: double defining the risk threshold beyond which a record/household needs to be swapped. This is understood as risk>=risk_threshhold.
  
  // initialise parameters
  int n=risk.size();
  int p=risk[0].size();
  std::vector<int> data_level(n);
  std::fill(data_level.begin(),data_level.end(),p);
  
  for(int i=0;i<n;i++){
    for(int j=0; j<p; j++){
      if(risk[i][j]>=risk_threshold){
        data_level[i] = j;
        break;
      }
    }
  }
  
  return data_level;
}

/*
 * Function to set to set risk for each individual 
 * in each hierarchy level
 * this is then used as smapling probability
 */
std::vector< std::vector<double> > setRisk(std::vector<std::vector<int> > &data, std::vector<int> &hierarchy, std::vector<int> &risk_variables, int &hid){
  
  // data: data input
  // hierarchy: column indices in data corresponding to geo hierarchy of data read left to right (left highest level - right lowest level)
  // risk_variables: column indices in data corresponding to risk variables which will be considered for estimating counts in the population
  // hid: int correspondig to column index in data which holds the household ID
  
  // initialise parameters
  int n = data[0].size();
  int nhier = hierarchy.size();
  int nrisk = risk_variables.size();
  // needed to temporarily store the risk of an individual in each
  // hierarchy
  std::vector<double> risk_value(nhier);
  int current_ID;
  int hsize=0;
  // initialise risk data
  // prob[0] ~ risk of first record for each hierarchy level
  // prob[1] ~ risk of second record for each hierarchy level
  // and so on ...
  std::vector< std::vector<double> > prob(n,vector<double>(nhier));
  
  //
  std::vector<int> loop_index = risk_variables;
  loop_index.insert(loop_index.end(),hierarchy.begin(),hierarchy.end());
  int loop_n = loop_index.size();
  std::vector<int> groups(loop_n);
  
  // initialise counts for groups in every hierarchy
  std::map<std::vector<int>,int> group_count; 
  for(int i=0;i<n;i++){
    
    for(int j=0;j<loop_n;j++){
      // ... define group for each hierarchy level
      // risk_variable + hierarchy levels
      groups[j] = data[loop_index[j]][i];
    }
    
    for(int index_hier=0;index_hier<nhier;index_hier++){
      std::vector<int> groups_help(&groups[0],&groups[nrisk+index_hier+1]); // +1 needed here!
      // ... count number for each group using std::map
      group_count[groups_help]++;
    }
  }
  
  // loop over data again and fill with risks
  int i=0;
  int h=0;
  while(i<n){
    
    current_ID = data[hid][i];
    while(i+h<n&&current_ID==data[hid][i+h]){
      
      for(int j=0;j<loop_n;j++){
        // ... define group for each hierarchy level
        // risk_variable + hierarchy levels
        groups[j] = data[loop_index[j]][i+h];
      }
      
      for(int index_hier=0;index_hier<nhier;index_hier++){
        // get each grouping ~ risk_variables + hierarchy level 0-nhier
        std::vector<int> groups_help(&groups[0],&groups[nrisk+index_hier+1]); //+1 needed here
        // select highest risk for hid in each hierarchy level
        risk_value[index_hier] = max(risk_value[index_hier],1.0/group_count[groups_help]);
      }
      
      hsize++;
      h++;
    }
    
    // assign highst risk and revers risk to all household members
    for(int j=0;j<hsize;j++){
      prob[i+j] = risk_value;
    }
    
    // reset parameters
    i = i+hsize;
    hsize=0;
    std::fill(risk_value.begin(),risk_value.end(),0.0);
    h =0;
  }
  
  return prob;
}



/*
 * Sampling function
 * samples the unordered indices in ID
 */
std::vector<int> randSample(std::unordered_set<int> &ID, int N, std::vector<double> &prob, std::mt19937 &mersenne_engine,
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
  std::priority_queue<std::pair<double, int> > q;
  std::vector<int> sampleID(ID.size());
  
  int z = 0;
  for(auto s : ID){
    if(IDused[s]==0){
      if(mustSwap.find(s)!=mustSwap.end()){
        sampleID[z]=s;
        z++;
      }else if(N>0){
        q.push(std::pair<double, int>(prob[s]/exp_dist(mersenne_engine), s));
      }
    }
  }
  // resize sampling vector 
  // z values are now in Vector + N are still to come
  N = max(0,min<int>(q.size(),N-z));
  sampleID.resize(N+z);
  
  // build output vector
  if(N>0){
    // select index of top elements from priority_queue
    for(int i=0;i<N;i++){
      sampleID[z+i] = q.top().second; //.top() access top element in queue
      q.pop(); // remove top element in queue
    }
  }
  return sampleID;
}


/*
 * Function to distribute n draws over a given number of groups
 * the distribution is always proportional to group size
 */
std::map<std::vector<int>,std::pair<int,int>> distributeDraws(std::map<std::vector<int>,std::unordered_set<int> > &group_hier,
                                                              int &nhid, double &swaprate,
                                                              std::uniform_int_distribution<std::mt19937::result_type> &runif01,
                                                              std::mt19937 &mersenne_engine){
  // group_hier map which contains all household indices per hierarchy level (only all hierarchy levels are used atm)
  // nhid int containing number of households in total
  // swaprate double containing the swaprate
  // runif01 & mersenne_engine for sampling procedures
  
  
  // swaprate/2 ensures that in the end this percentage of households is swapped
  // so 1 swap is counted double since with each swap 2 households are swapped
  int total_swaps = 0; 
  if(runif01(mersenne_engine)==0){
    total_swaps = ceil(nhid*(swaprate/2));
  }else{
    total_swaps = floor(nhid*(swaprate/2));
  }
  // cout << "total swaps:" << total_swaps<<endl;
  // distribute them among smallest hierarchy level
  // draw_group[key].first correponds to number of households
  // draw_group[key].second to number of swaps
  std::map<std::vector<int>,std::pair<int,int>> draw_group;
  double draw_excess_help = 0;
  double x_excess = 0;
  for(auto const&x : group_hier){
    draw_group[x.first].first = x.second.size(); // this is needed later on
    
    x_excess = (double)x.second.size()/(double)nhid*(double)total_swaps;
    
    draw_group[x.first].second = floor(x_excess);
    
    x_excess = x_excess-floor(x_excess);
    draw_excess_help = draw_excess_help + x_excess;
  }
  
  int draw_excess = std::round(draw_excess_help);
  // randomly shuffeld index vector
  // this is similar to randomly round up and down in each group so on average swaprate will be reached
  std::vector<int> add_extra(group_hier.size());
  std::iota(add_extra.begin(),add_extra.end(),0);
  std::shuffle(add_extra.begin(),add_extra.end(),mersenne_engine);
  std::sort(add_extra.begin(),add_extra.begin()+draw_excess); // sort first draw_excess elemets in vector
  
  // pick first draw_excess values and add one to them
  int z = 0;
  int v = 0;
  int count_swaps=0;
  // certain groups will get one more draw
  for(auto const&x : draw_group){
    if(add_extra[v]==z){
      draw_group[x.first].second++;
      v++;
    }
    count_swaps = count_swaps+draw_group[x.first].second;
    if(v>(draw_excess-1)){
      break; // if all draw_excess have been distributed break procedure
    }
    z++;
  }
  
  return draw_group;
}


/*
* Function to sample from donor set
* this is done differently than the inital sampling to make procedure more efficient
*/
std::vector<int> sampleDonor(std::vector< std::vector<int> > &data, std::vector<int> &similar,
                             std::vector<int> &IDswap, std::unordered_set<int> &IDswap_pool,
                             std::map<double,int> &IDdonor_pool, std::vector<int> &IDused, int &hid){
  
  // data: data input data.[0].size ~ number of records - data.size ~ number of varaibles per record
  // similar: column indices in data corresponding to variables (household/personal) which should be considered when swapping,
  // e.g. swapping onlys household with same houshoeld size 
  // IDswap: vector containing household IDs to be swapped
  // IDswap_pool: unordered set containing sampling pool from which IDswap was drawn
  // IDdono_pool: map containing every possible donor ID (ordered by sampling probability in ascending order)
  // IDused: integer vector which takes on 1 if ID was sampled
  
  // define parameter
  std::vector<int> IDdonor(IDswap.size(),-1); // output initialize with -1
  // if value stays -1 then no donor was found for corresponding value in IDswap
  bool similar_true=true; 
  int index_donor = 0;
  
  // select donor based on similarity constrains
  // iterate over both unordered sets
  // iterate over IDdonor_pool in reverse order since it is sorted in ascending order by risk
  for(int i=0; i<IDswap.size();i++){
    // find donor for index_samp
    // iterate over complete donor set in reverse order
    for( auto it = IDdonor_pool.end();it!=IDdonor_pool.begin(); ){
      // it->second access the value
      // it->first access the key
      it--; // decrement iterator first since you loop from the back
      index_donor = it->second;
      // if was not used and it is not in the same hierarchy ~ IDswap_pool.find(s.second)==IDswap_pool.end()
      // it is a possible donor
      if(IDused[index_donor]==0 && IDswap_pool.find(index_donor)==IDswap_pool.end()){
        // IDswap[i] is similar to index_donor
        // by using similarity indices of the profile
        similar_true=true;
        for(int sim=0;sim<similar.size();sim++){
          if(data[similar[sim]][IDswap[i]]!=data[similar[sim]][index_donor]){
            // similarity variables do not match
            // set similar_true=false and break loop
            similar_true=false;
            break;
          }
        }
        if(similar_true){
          IDdonor[i] = index_donor;
          IDused[it->second] = 1;
          // if index_donor was used
          // remove it from IDdonor_pool and do not increment it
          IDdonor_pool.erase(it);
          goto next_index_samp;
        }
      }
    }
    // must have ; after the goto label since goto label is not a statement
    next_index_samp:
      ;
  }
  
  return IDdonor;  
}


/*
* Function to perform record swapping
*/
std::vector< std::vector<int> > recordSwap(std::vector< std::vector<int> > data, std::vector<int> similar,
                                         std::vector<int> hierarchy, std::vector<int> risk, int hid, int th, double swaprate,
                                         int seed = 123456){
  
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
  std::unordered_set<int> IDnotUsed;
  // needed for running random number generator and
  // set random seed according to input parameter
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  // initialise lambda para for exp distribution
  std::exponential_distribution<double> exp_dist(1.0);
  
  // initialize random number generator rounding up and down in the procedure
  std::uniform_int_distribution<std::mt19937::result_type> runif01(0,1);
  
  std::vector<int> levels(n);

  
  ////////////////////////////////////////////////////
  // order data by hid 
  // not needed at the moment -> order data outside function
  // orderData(data,hid);
  ////////////////////////////////////////////////////
  
  
  ////////////////////////////////////////////////////
  // define risk data if not supplied by user
  // using risk_variables and 1/counts
  std::vector< std::vector<double> > prob(n,std::vector<double>(nhier));
  prob = setRisk(data, hierarchy, risk, hid);
  ////////////////////////////////////////////////////
  
  ////////////////////////////////////////////////////
  // define minimum swap level for each household
  double risk_threshold = 0;
  if(th==0){
    risk_threshold = 2.0;
  }else{
    risk_threshold = 1.0/(double)th;
  }
  levels = setLevels(prob,risk_threshold);
  
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
  
  std::map<std::vector<int>,std::unordered_set<int> > group_hier; //
  std::unordered_map<int,std::unordered_set<int> > group_levels; // map containing all IDs which must be swapped at a certain level (~key of map)
  std::vector<int> hier_help(nhier); // help vector to get hierarchy groups
  std::unordered_map<int,int> swappedIndex; // map for indices that have already been used -> unorderd map constant time access
  std::vector<int> IDused(n,0); // 0-1 vector if 1 this index was already swapped and cant be swapped again
  std::unordered_set<int> IDdonor_all; // set for all IDs for quick lookup
  std::map<int,std::map<double,int>> samp_order_donor;
  int z=0; // counter used for while() ect...
  int nhid = 0;
  
  /////////////////////////////
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
    group_hier[hier_help].insert(z);
    
    // create map for levels
    if(levels[z]<nhier){
      group_levels[levels[z]].insert(z);
    }
    
    // create set of IDs for quick lookup
    IDdonor_all.insert(z);
    
    // create map for random numbers (ordered)
    // makes sampling in each iteration obsolete
    // look up in these maps instead
    for(int j=0;j<nhier;j++){
      samp_order_donor[j][prob[z][j]/exp_dist(mersenne_engine)] = z;
    }
    
    // count number of households
    nhid++;
    // skip all other household member, only need first one
    z += map_hsize[data[hid][z]];
    
  }
  /////////////////////////////

  /////////////////////////////
  // get number of households to be swapped at the lowest level hierarchy
  // this is only used at lowest hierarchy level
  // draw_group[].first -> number of households in lowest level hierarchy
  // draw_group[].second -> number of swaps in lowest level hierarchy
  std::map<std::vector<int>,std::pair<int,int>> draw_group =  distributeDraws(group_hier, nhid, swaprate, 
                                                                              runif01, mersenne_engine);
  
  /////////////////////////////
  int check_donor = 0;
  int check_sample = 0;
  
  /////////
  // this is needed only for the lowest hierarchy
  // will be changed for the final version
  std::vector<double> prob_help(n);
  for(int i=0;i<n;i++){
    prob_help[i] = prob[i][nhier-1];
  }

  /////////////////////////////
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
    }
    
    std::map<std::vector<int>,unordered_set<int> > group_hier_help;
    hier_help.resize(h+1);
    
    /////////////////
    // get combined map for hierarchy h
    for(auto const&x : group_hier){
      
      // get higher hierarchy
      std::copy(x.first.begin(),x.first.begin()+h+1,hier_help.begin());
      // for(int i=0;i<h+1;i++){
      //   hier_help[i] = x.first[i];
      // }
      
      // discard every index that has already been used
      // more efficient to do this at this step then later on in the code
      for(auto s : x.second){
        if(IDused[s]==0){
          group_hier_help[hier_help].insert(s);
        }
      }
    }
    /////////////////
    
    
    /////////////////

    int countUsed=0;
    int countRest=0;
    int sampSize=0;
    /////////////////
    // loop over levels of hierarchy
    for(auto &x : group_hier_help){
      // std::vector<int> xfirst = group_hier_help.begin()->first;
      // std::vector<int> xsecond = group_hier_help.begin()->second;
      // get values that need to be swapped at this hierarchy level and which are
      // in this hierarchy stage
      std::vector<int> IDswap(x.second.size());
      
      
      if(h<(nhier-1)){
        // in all but the last hierarchy level do the follwing: 
        if(mustSwap.size()>0){
          // loop over values in x.second
          z=0;
          for(auto s : x.second){
            // if there are any households that must be swapped due to variable k_anonymity/risk_threshold
            if(IDused[s]==0 && mustSwap.find(s)!=mustSwap.end()){
              IDswap[z]=s;
              z++;
            }
          }
          IDswap.resize(z);
        }else{
          IDswap.resize(0); // nothing needs to be swapped
        }
      }else{
        // in the last hierarchy level do the following:
        // if at lowest level get number of households that need to be swapped
        // according to swap and check if this number was already reached
        // by previous swappings
        countUsed = draw_group[x.first].first - x.second.size();
        
        // not enough households have been swapped
        // when checking at lowest level
        // Number of IDs that need to be swapped - already swapped IDs - IDs that have to be swapped at lowest level:
        countRest = draw_group[x.first].second - countUsed;
        countRest = std::max(0,countRest);

        std::unordered_set<int> IDswap_draw = x.second;
        
        // apply sampling here -> should still be quick because IDswap_draw will not be extremely large
        // in randSample households that must be swapped are automatically choosen
        std::vector<int> IDswap_help = randSample(IDswap_draw,countRest,prob_help,mersenne_engine,IDused,mustSwap);
        IDswap.resize(IDswap_help.size());
        IDswap = IDswap_help; 
      }

      // if any IDs need to be swapped:
      if(IDswap.size()>0){
        sampSize = sampSize+IDswap.size();
        // get donor set
        // if IDdonor is -1 at a position ==> no donor for IDswap at same position
        std::vector<int> IDdonor = sampleDonor(data, similar, IDswap, x.second,
                                               samp_order_donor[h], IDused, hid);
        
        // set Index to used
        for(int i=0;i<IDdonor.size();i++){
          if(IDdonor[i]>-1){
            IDused[IDdonor[i]]=1;
            IDused[IDswap[i]]=1;
            // store results from sampling in swappedIndex 
            swappedIndex[IDswap[i]] = IDdonor[i];
          }else{
            IDnotUsed.insert(IDswap[i]);
          }
        }
        
      }
      /////////////////
    }
  }
  
  ////////////////////////////////////////////////////
  // Create output using swappedIndex

  int swap_hierarchy,swap_hierarchy_with;
  int hsize=0;
  int hsizewith=0;
  for(auto const&x : swappedIndex){
    hsize = map_hsize[data[hid][x.first]];
    hsizewith = map_hsize[data[hid][x.second]];

    // loop over hierarchy
    for(int j=0;j<nhier;j++){
      swap_hierarchy = data[hierarchy[j]][x.first];
      swap_hierarchy_with = data[hierarchy[j]][x.second];
      for(int h=0;h<max(hsize,hsizewith);h++){
        // swap hierarchy for every household member in x.first
        if(x.first+h>=n){
          cout<<data[hid][x.first]<<endl;
          break;
        }
        if(x.second+h>=n){
          cout<<data[hid][x.second]<<endl;
          break;
        }
        if(h<hsize){
          data[hierarchy[j]][x.first+h] = swap_hierarchy_with;
        }
        // swap hierarchy for every household member in x.second
        if(h<hsizewith){
          data[hierarchy[j]][x.second+h] = swap_hierarchy;
        }
      }
    }
  }
  
  return data;
  
}


