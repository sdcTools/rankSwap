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

mb_all <- list()
for(n in npop){
  for(h in hier.types){
    dat <- recordSwapping:::create.dat(n)
    
    hierarchy <- c("nuts1","nuts2","nuts3","nuts4")
    hierarchy <- hierarchy[1:h]
    risk_variables <- c("ageGroup","geschl","hsize","national")
    swaprate <- .05 # runif(1)
    hid <- "hid"
    k_anonymity <- 3
    nhier <- length(hierarchy)
    similar <- list(c(5))

    mb <- microbenchmark(cpp=recordSwap(dat,similar,0:(nhier-1),5:8,4,k_anonymity,swaprate,seed=sample(1:1e6,1)),
                         times=times)
    mb <- as.data.table(mb)
    mb[,npop:=n]
    mb[,hier.levels:=h]
    save(mb,file=paste0("R/benchmark_",n,"_",h,".RData"))
    mb_all <- c(mb_all,list(mb))
  } 
}


# mb_all <- rbindlist(mb_all)
# save(mb_all,file="R/benchmark.RData")
save(mb_all,file="R/benchmark_cpp.RData")
