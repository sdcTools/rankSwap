#' @title Targeted Record Swapping
#' 
#'  
#' @description Applies targeted record swapping on micro data considering the identification risk of each record as well the geographic topology.
#'  
#' @details The procedure accepts a `data.frame` or `data.table` containing all necessary information for the record swapping, e.g parameter `hid`, `similar`, `hierarchy`, ect... \cr 
#' First the micro data in `data` is ordered by `hid` and the identification risk is calculated for each record in each hierarchy level. As of right now only counts is used as 
#' identification risk and the inverse of counts is used as sampling probability.\cr
#' NOTE: It will be possible to supply an identification risk for each user and hierarchy level which will be passed down to the C++-function. This is however not fully implemented.
#' \cr
#' \cr 
#' With the parameter `th` a k-anonymity rule is applied to define risky households in each hierarchy level. A household is set to risky if counts <= th in any hierarchy level and the household needs to be swapped across this hierarchy level.
#' For instance having a geographic hierarchy of county > municipality > district the counts are calculated for each geographic variable and defined `risk_variables`. If the counts for a record falls below `th` for hierachy county then this record
#' needs to be swapped accross counties.\cr
#' Setting `th = 0` disables this feature and no risky households are defined.
#' \cr 
#' After that the targeted record swapping is applied starting from the highest to the lowest hierarchy level and cycling through all possible geographic areas at each hierarchy level, e.g every county, every municipality in ever county, ect...
#' At each geographic area a set of values is created for records to be swapped.\cr
#' In all but the lowest hierarchy level this is ONLY made out of all records which do not fullfill the k-anonymity and have not already been swapped.
#' Those records are swapped with records not belonging to the same geographic area, which have not already been swapped before hand. Swapping referes to the interchange of geographic variables defined in `hierarchy`.
#' When a record is swapped all other record containing the same `hid` are swapped as well. \cr
#' \cr 
#' At the lowest hierarchy level in every geographic area the set of records to be swapped is made up of all records which do not fullfill the k-anonymity as well as the remaining numer of records such
#' that the proportion of swapped records of the geographic area is in coherence with the `swaprate`. If, due to the k-anonymity condition, more records have already been swapped in this geographic area then only the 
#' records which do not fullfill the k-anonymity are swapped.
#' \cr
#' `swaprate` sets the swaprate of households to be swapped, where a single swap counts for swapping 2 households, the sampled household and the corresponding donor.
#' Prior to the procedure the swaprate is applied on the lowest hierarchy level, to determine the target number of swapped households in each of the lowest hierarchies. If the target numbers of a decimal point they will randomly be 
#' rounded up or down such that the number of households swapped in total is in coherence to the swaprate.
#'  
#' @param data micro data set containing only integer values.
#' @param similar list of integer vectors containing similarity profiles, see details for more explanations.
#' @param hierarchy column indices of variables in `data` which refere to the geographic hierarchy in the micro data set. For instance county > municipality > district.
#' @param risk_variables column indices of variables in `data` which will be considered for estimating the risk.
#' @param hid column index in `data` which refers to the household identifier.
#' @param th integer defining the threshhold of high risk households (counts<k).
#' @param swaprate double between 0 and 1 defining the proportion of households which should be swapped, see details for more explanations
#' @param carry_along integer vector indicating additional variables to swap besides to hierarchy variables.
#' These variables do not interfere with the procedure of finding a record to swap with or calculating risk.
#' This parameter is only used at the end of the procedure when swapping the hierarchies.
#' @param seed integer defining the seed for the random number generator, for reproducability.
#' 
#' @return Returns data set with swapped records.
#' 
#' @export recordSwap
recordSwap <- function(data, similar, hierarchy, risk, hid, th, swaprate, carry_along = NULL, seed = 123456L){
  
  cnames <- copy(colnames(data))
  
  # order data by hid
  if(!"data.table"%in%class(data)){
    data <- as.data.table(data)
  }
  
  setkeyv(data,cnames[hid+1])
  
  # transpose data for cpp function
  # data <- transpose(data)
  # default values since those parameters are not used yet
  if(any(c(unlist(similar), hierarchy, risk, hid)>=ncol(data))){
    stop("Indices higher than column number in data")
  }
  
  
  data <- recordSwap_cpp(data, similar, hierarchy, risk, hid, th, swaprate, carry_along, seed)
  data <- as.data.table(data)
  setnames(data,colnames(data),cnames)
  
  return(data)
}
