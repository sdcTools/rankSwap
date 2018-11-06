###############################################################
# RUN MICROBENCHNMARK TEST
#
#

#library(data.table)
#library(Rcpp)
library(microbenchmark)
library(recordSwapping)
set.seed(1234)

#source("R/create_dat.R")
#sourceCpp("src/recordSwap.cpp")
###
# R-Function:

###

npop <- c(1,2^(1:5))*1e4
# npop <- c(1000,npop)
npop <- c(1000,npop,500000,1e6)
hier.types <- 2:4
times <- 100 # runtimes for microbenchmark
n <- npop[1]
mb_all <- list()
for(n in npop){
  for(h in hier.types){
    cat("n=",n," ")
    cat("hierarchies=",h,"\n")
    dat <- recordSwapping:::create.dat(n)
    
    hierarchy <- 0:3
    hierarchy <- hierarchy[1:h]
    risk_variables <- 7:8
    swaprate <- .05 # runif(1)
    hid <- 4
    k_anonymity <- 2
    nhier <- length(hierarchy)
    similar <- c(5)
    
    mb <- microbenchmark(cpp=recordSwapping:::recordSwap_cpp(dat, similar, hierarchy, risk_variables, hid, k_anonymity, swaprate, seed=sample(1:1e6,1)),
                         times=times)
    
    mb <- as.data.table(mb)
    mb[,npop:=n]
    mb[,hier.levels:=h]
    save(mb,file=paste0("data/benchmark_",n,"_",h,".RData"))
    mb_all <- c(mb_all,list(mb))
  } 
}


# mb_all <- rbindlist(mb_all)
# save(mb_all,file="data/benchmark.RData")
save(mb_all,file="data/benchmark_cpp.RData")
