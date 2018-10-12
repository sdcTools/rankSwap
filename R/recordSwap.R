#' @title Targeted Record Swapping
#' 
#'  
#' @description Applies targeted record swapping on micro data considering the identification risk of each record as well the geographic topology.
#'  
#' @details The procedure accepts a rectanglar data set containing all necessary information for the record swapping, e.g parameter \code{hid}, \code{similar}, \code{hierarchy}, ect... \cr 
#' First the micro data in \code{data} is ordered by \code{hid} and the identification risk is calculated for each record. As of right now only counts is used as 
#' identification risk and the inverse of counts is used as sampling probability. \cr
#' \cr 
#' In addition each record gets an index corresponding to the hierarchy level over which the household must be swapped. For instance having a geographic hierarchy
#' of county > municipality > district the risk is calculated for each geographic variable and defined risk variables. If a record falls below \code{th} for hierachy county then this record
#' needs to be swapped accross counties. \cr
#' \cr 
#' After that the targeted record swapping is applied starting from the highest to the lowest hierarchy level and cycling through all possible geographic areas at each hierarchy level, e.g every county, every municipality in ever county, ect...
#' At each geographic area a set of values is created for records to be swapped.\cr
#' In all but the lowest hierarchy level this is made out of all records which do not fullfill the k-anonymity and have not already been swapped. k-anonymity is implemented in the sense that a household will be swapped if \code{counts<=th}.
#' Those records are swapped with records not belonging to the same geographic area, which have not already been swapped before hand. Swapping referes to the interchange of geographic variables defined in \code{hierarchy}.
#' When a record is swapped all other record containing the same \code{hid} are swapped as well. \cr
#' \cr 
#' At the lowest hierarchy level in every geographic area the set of records to be swapped is made up of all records which do not fullfill the k-anonymity as well as the remaining numer of records such
#' that the proportion of swapped records of the geographic area is equal to \code{swaprate}. If, due to the k-anonymity condition, more records have already been swapped in this geographic area then only the 
#' records which do not fullfill the k-anonymity are swapped.
#' 
#' @param data micro data set containing only integer values.
#' @param similar column indices of variables in \code{data} which should be considered when swapping households, see details for more explanations
#' @param hierarchy column indices of variables in \code{data} which refere to the geographic hierarchy in the micro data set. For instance county > municipality > district.
#' @param risk column indices of variables in \code{data} which will be considered for estimating the risk.
#' @param hid column index in \code{data} which refers to the household identifier.
#' @param th integer defining the threshhold of high risk households (k-anonymity).
#' @param swaprate double between 0 and 1 defining the proportion of households which should be swapped, see details for more explanations
#' @param seed integer defining the seed for the random number generator, for reproducability.
#' 
#' @return Returns data set with swapped records.
#' 
#' @export recordSwap
recordSwap <- function(data, similar, hierarchy, risk, hid, th, swaprate, seed = 123456L){

  # default values since those parameters are not used yet
  risk_threshold <- 0
  risk <- data.frame(rnorm(10))
  
  recordSwap_cpp(data, similar, hierarchy, risk_variables, hid, k_anonymity, swaprate, risk_threshold, riks, seed)
}