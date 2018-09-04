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
npop <- c(1000,npop,500000,1e6,2e6)
hier.types <- 2:4

mb_all <- list()
for(n in npop){
  for(h in hier.types){
    dat <- recordSwapping:::create.dat(n)
    
    hierarchy <- c("nuts1","nuts2","nuts3","nuts4")
    hierarchy <- hierarchy[1:h]
    risk <- c("ageGroup","geschl","hsize","national")
    swap <- .05 # runif(1)
    hid <- "hid"
    th <- 3
    nhier <- length(hierarchy)
    
    levels <- recordSwapping:::setLevels_cpp(dat,0:(nhier-1),5:8,4,th)
    prob <- recordSwapping:::setRisk_cpp(dat,0:(nhier-1),5:8,4)
    
    # prep data for R function
    dat_R <- copy(dat)
    dat_R[,level:=levels]
    dat_R[,risk:=prob[[1]]]
    dat_R[,antirisk:=prob[[2]]]
    dat_R <- unique(dat_R,by="hid")
    
    for(i in 1:length(hierarchy)){
      if(i==1){
        dat.draw <- dat_R[,.(drawN=recordSwapping:::randomRound(.N*swap)),by=c(hierarchy[1:i],"hsize")]
      }else{
        # distribute N
        # take number of risky households into account??????!!!!!!!!
        dat.draw.next <- dat_R[,.(N=as.numeric(.N)),by=c(hierarchy[1:i],"hsize")]
        dat.draw.next[,N:=N/sum(N),by=c("hsize",hierarchy[1:(i-1)])]
        dat.draw <- merge(dat.draw,dat.draw.next,by=c("hsize",hierarchy[1:(i-1)]),all.y=TRUE)
        dat.draw[,drawN:=recordSwapping:::randomRound(N*drawN),by=c(hierarchy[1:i],"hsize")]
        dat.draw[,N:=NULL]
      }
    }
    dat_R <- merge(dat_R,dat.draw,by=c(hierarchy,"hsize"))
    
    
    mb <- microbenchmark(cpp=recordSwap(dat,5,0:(nhier-1),5:8,4,th,swap),
                         # R=recordSwapR(copy(dat_R),hierarchy),
                         times=50)
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
