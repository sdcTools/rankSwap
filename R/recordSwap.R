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
#' With the parameter `k_anonymity` a k-anonymity rule is applied to define risky households in each hierarchy level. A household is set to risky if counts <= k_anonymity in any hierarchy level and the household needs to be swapped across this hierarchy level.
#' For instance having a geographic hierarchy of county > municipality > district the counts are calculated for each geographic variable and defined `risk_variables`. If the counts for a record falls below `k_anonymity` for hierachy county then this record
#' needs to be swapped across counties.\cr
#' Setting `k_anonymity = 0` disables this feature and no risky households are defined.
#' \cr 
#' After that the targeted record swapping is applied starting from the highest to the lowest hierarchy level and cycling through all possible geographic areas at each hierarchy level, e.g every county, every municipality in ever county, ect...
#' At each geographic area a set of values is created for records to be swapped.\cr
#' In all but the lowest hierarchy level this is ONLY made out of all records which do not fulfil the k-anonymity and have not already been swapped.
#' Those records are swapped with records not belonging to the same geographic area, which have not already been swapped before hand. Swapping refers to the interchange of geographic variables defined in `hierarchy`.
#' When a record is swapped all other record containing the same `hid` are swapped as well. \cr
#' \cr 
#' At the lowest hierarchy level in every geographic area the set of records to be swapped is made up of all records which do not fulfil the k-anonymity as well as the remaining numer of records such
#' that the proportion of swapped records of the geographic area is in coherence with the `swaprate`. If, due to the k-anonymity condition, more records have already been swapped in this geographic area then only the 
#' records which do not fulfil the k-anonymity are swapped.
#' \cr
#' Using the parameter `similar` one can define similarity profiles. `similar` needs to be a list of vectors with each list entry containing column indices of `data`. These entries are be used when searching for donor households,
#' meaning that for a specific record the set of all donor records is made out of records which have the same values in `similar[[1]]`. IMPORTANT: These can only be household variables
#' \cr
#' If no suitable donor can be found the next similarity profile is used, `similar[[2]]` and
#' the set of all donors is then made up out of all records which have the samle values in the column indices in `similar[[2]]`. This procedure continues until a donor record was found or all the similarity profiles have been used.
#' \cr
#' `swaprate` sets the swaprate of households to be swapped, where a single swap counts for swapping 2 households, the sampled household and the corresponding donor.
#' Prior to the procedure the swaprate is applied on the lowest hierarchy level, to determine the target number of swapped households in each of the lowest hierarchies. If the target numbers of a decimal point they will randomly be 
#' rounded up or down such that the number of households swapped in total is in coherence to the swaprate.
#'  
#' @param data micro data set containing only integer values, must be either `data.table` or `data.frame`.
#' @param hid column index or column name in `data` which refers to the household identifier.
#' @param hierarchy column indices or column names of variables in `data` which refer to the geographic hierarchy in the micro data set. For instance county > municipality > district.
#' @param similar vector or list of integer vectors or column names containing similarity profiles, see details for more explanations.
#' @param swaprate double between 0 and 1 defining the proportion of households which should be swapped, see details for more explanations
#' @param risk either column indices or column names in `data` or `data.table`, `data.frame` or `matrix` indicating risk of each record at each hierarchy level. If `risk`-matrix is supplied to swapping procedure will
#' not use the k-anonymity rule but the values found in this matrix for swapping.
#' ATTENTION: This is NOT fully implemented yet and currently ignored by the underlying c++ functions until tested properly
#' @param risk_threshold single numeric value indicating when a household is considered "high risk", e.g. when this household must be swapped.
#' Is only used when `risk` is not `NULL`.
#' ATTENTION: This is NOT fully implemented yet and currently ignored by the underlying c++ functions until tested properly
#' @param k_anonymity integer defining the threshold of high risk households (counts<k) for using k-anonymity rule
#' @param risk_variables column indices or column names of variables in `data` which will be considered for estimating the risk.
#' Only used when k-anonymity rule is applied.
#' @param carry_along integer vector indicating additional variables to swap besides to hierarchy variables.
#' These variables do not interfere with the procedure of finding a record to swap with or calculating risk.
#' This parameter is only used at the end of the procedure when swapping the hierarchies.
#' @param return_swapped_id, boolean if `TRUE` the output includes an additional column showing the `hid` with which a record was swapped with.
#' The new column will have the name `paste0(hid,"_swapped")`.  
#' @param seed integer defining the seed for the random number generator, for reproducibility. if `NULL` a random seed will be set using `sample(1e5,1)`.
#' 
#' @return `data.table` with swapped records.
#' 
#' @export recordSwap
recordSwap <- function(data, hid, hierarchy, similar,
                       swaprate=0.05, risk=NULL, risk_threshold=0,
                       k_anonymity=3, risk_variables=NULL,
                       carry_along = NULL,
                       return_swapped_id = FALSE,
                       seed = NULL){
  
  # check data
  if(all(!class(data)%in%c("data.table","data.frame"))){
    stop("data must be either a data.table, data.frame")
  }
  
  data <- as.data.table(data)
  cnames <- copy(colnames(data))
  
  ##########################
  # # check inputs

  # check hid
  hid <- checkIndexString(hid,cnames,matchLength = 1)
  
  # check hierarchy
  hierarchy <- checkIndexString(hierarchy,cnames,minLength = 1)
  
  # check similar
  if(!is.list(similar)){
    similar <- list(similar)
  }
  similar <- lapply(similar,checkIndexString,cnames=cnames,minLength = 1)
  
  # check risk_variables
  risk_variables <- checkIndexString(risk_variables,cnames,minLength = 1)
  
  # check carry_along
  carry_along <- checkIndexString(carry_along,cnames,minLength = 0)
  
  # check return_swapped_id and use with carry_along if TRUE
  if(!is.logical(return_swapped_id) | length(return_swapped_id)!=1){
    stop("return_swapped_id must be logical of length 1")
  }
  
  if(return_swapped_id==TRUE){
    orig_id <- cnames[hid+1]
    swapped_id <- paste0(orig_id,"_swapped")
    data[,c(swapped_id):=get(orig_id)]
    cnames <- copy(colnames(data))
    
    swapped_id <- checkIndexString(swapped_id,cnames,
                                   matchLength = 1)
    carry_along <- c(carry_along,swapped_id)
  }
  
  # check k_anonymity
  if(!((!is.null(risk_variables))&&checkInteger(k_anonymity)&&length(k_anonymity)==1&&k_anonymity>=0)){
    stop("k_anonymity must be a positiv single integer!")
  }
  
  # check risk_threshold
  if(is.null(risk_variables)){
    if(!(is.numeric(risk_threshold)&&length(risk_threshold)==1&&risk_threshold>=0)){
      stop("risk_threshold must be a positiv numeric value")
    }
  }

  # check swaprate
  if(!(is.numeric(swaprate)&&length(swaprate)==1&&swaprate%between%c(0,1))){
    stop("swaprate must be a single number between 0 and 1!")
  }
  
  # check risk
  if(is.null(risk)){
    risk <- data.table()
    risk_threshold <- 0
  }
  if(is.vector(risk)){
    if(length(risk)!=length(hierarchy)){
      stop("risk and hierarchy need to address the same number of columns!")
    }
    risk <- checkIndexString(risk,cnames,minLength = 1)
    risk <- data[,c(risk+1)]
  }else{
    if(all(!class(risk)%in%c("data.table","data.frame","matrix"))){
      stop("If risk is not a vector containing column indices or column names in data then risk must be either a data.table, data.frame or matrix!")
    }
  }

  if(nrow(risk)>0){
    if(ncol(risk)!=length(hierarchy)){
      stop("number of columns in risk does not match number of hierarchies!")
    }
  }

  cnamesrisk <- copy(colnames(risk))
  risk <- data.table(risk)
  
  if(nrow(risk)>0){
    if(is.null(cnamesrisk)){
      message("risk does not contain column names; the first column in risk will be used for the first hierarchy level, e.g ",cnames[hierarchy[1]+1]," and so on.")
    }else{
      if(!any(cnamesrisk)%in%cnames[hierarchy+1]){
        stop("the columnnames of risk do not appear in data")
      }
    }
    
    if(any(risk<0)||any(!is.numeric(risk))){
      stop("risk must contain positive real values only!")
    }
  }

  # check seed
  if(is.null(seed)){
    seed <- sample(1e5,1)
  }
  if(!(seed%%1==0&&length(seed)==1&&seed>0)){
    stop("seed must be a single positive integer!")
  }

  ##########################
  # setup data and inputs for c++ function
  
  # order data
  setkeyv(data,cnames[hid+1])
  # take sub data 
  data[,helpVariableforMergingAfterTRS:=.I]
  sim_vars <- sort(unique(unlist(similar)))
  original_cols <- unique(c(hid,hierarchy,risk_variables,sim_vars,carry_along))
  select_cols <- unique(c(original_cols+1,ncol(data)))
  data_sw <- copy(data[,..select_cols])
  cnames_sw <- colnames(data_sw) # save column names for later use
  # remove columns from original data except help variable for merging
  drop_cols <- cnames_sw[-length(cnames_sw)]
  data[,c(drop_cols):=NULL]
  
  # remap column indices
  hid <- which(hid %in% original_cols)-1
  hierarchy <- sapply(hierarchy,function(z){
    which(original_cols %in% z) -1
  })

  if(length(similar)>0){
    # remap all similarity variables
    similar <- lapply(similar,function(z){
      sapply(z,function(z.s){
        which(original_cols %in% z) -1
      })
    })
  }
  if(length(risk_variables)>0){
    risk_variables <- sapply(risk_variables,function(z){
      which(original_cols %in% z) -1
    })
  }
  if(length(carry_along)>0){
    carry_along <- sapply(carry_along,function(z){
      which(original_cols %in% z) -1
    })
  }
  
  # check if any non numeric values are present in data
  if(any(!unlist(apply(data_sw,2,is.numeric)))){
    stop("Columns specified in hid, hierarchy, similar and carry_along must contain only integer values at this point")
  }
  
  # check if any values with decimal values are present in data
  decOccured <- apply(data_sw,2,function(z){any((z%%1)!=0)})
  if(any(decOccured)){
    decOccured <- names(decOccured)[decOccured]
    stop("data must contain only integer values.\nColumn(s)\n    ",paste(decOccured,collapse=", "),"\ncontain(s) decimal numbers")
  }
  
  
  # transpose data for cpp function
  data_sw <- transpose(data_sw)
  
  # transpose risk
  if(nrow(risk)>0){
    risk <- transpose(risk)
  }else{
    risk <- numeric(0)
  }
  risk <- numeric(0) # drop this if risk was tested enough

  data_sw <- recordSwap_cpp(data=data_sw, similar=similar, hierarchy=hierarchy,
                         risk_variables=risk_variables, hid=hid, k_anonymity=k_anonymity,
                         swaprate=swaprate,
                         risk_threshold=0, risk=risk,
                         carry_along = carry_along,
                         seed=seed)
  setDT(data_sw)
  data_sw <- transpose(data_sw)
  setnames(data_sw,colnames(data_sw),cnames_sw)
  data[data_sw,c(drop_cols):=mget(drop_cols),on=.(helpVariableforMergingAfterTRS)]
  rm(data_sw)
  setcolorder(data,cnames)
  data[,helpVariableforMergingAfterTRS:=NULL]
  
  return(data)
}



# helpfunction to check if inputs are correct

# check if integer or string and if length is appropriate
checkInteger <- function(x){
  if(is.numeric(x)){
    return(all(x%%1==0))
  }else{
    return(FALSE)
  }
}

# check length and type and convert to integer position for c++ function
checkIndexString <- function(x=NULL,cnames,matchLength=NULL,minLength=NULL){
  
  # return numeric(0) of input is null
  if(is.null(x) & (is.null(minLength)||minLength==0)){
    return(numeric(0))
  }
  
  varName <- deparse(substitute(x))
  
  if(!is.null(matchLength)){
    if(!((checkInteger(x)|is.character(x))&length(x)==matchLength)){
      stop(varName," must be an integer (column index) or character (column name) of length ",matchLength)
    }
  }else{
    if(!((checkInteger(x)|is.character(x))&length(x)>=minLength)){
      stop(varName," must contain integers (column indices) or characters (~column names) of data")
    }
  }
  
  # check when string that names are in cnames
  if(all(is.character(x))){
    if(any(!x%in%cnames)){
      stop("Column name(s) in ",varName," are not found in data")
    }
    # initialize index
    x <- match(x,cnames)
  }
  
  # check that index does not
  # - exceed number of column of data
  # x must be integer from this point onwards
  if(any(x>length(cnames))){
    stop("Column index in ",varName," exceeds number of columns in data")
  }
  if(any(x==0)){
    stop("Index in ",varName," does contain zero.\nIndexing in R starts with 1!")
  }
  if(any(x<0)){
    stop("Column indices cannot be negative")
  }
  
  # indices start with 0 for c++ routine
  x <- x-1
  return(x)
}
