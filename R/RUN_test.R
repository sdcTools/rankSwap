###############################################################
# RUN MICROBENCHNMARK TEST
#
#

library(data.table)
library(Rcpp)
library(microbenchmark)
set.seed(1234)

source("R/create_dat.R")
sourceCpp("src/recordSwap.cpp")
###
# R-Function:
recordSwapR <- function(dat,hierarchy){
  
  dat[,SELECTED:=0]
  dat[,ID:=.I]
  nhier <- length(hierarchy)
  for( i in 1:length(hierarchy)){
    hier.i <- unique(dat[,c(hierarchy[1:i]),with=FALSE])
    
    if(i<nhier){
      # upper stages
      for(h in 1:nrow(hier.i)){
        select.ih <- dat[hier.i[h],on=colnames(hier.i)][level==i&SELECTED==0,ID]
        swap.with <- dat[!hier.i[h],on=colnames(hier.i)[1:i]][SELECTED==0,.(ID,antirisk)]
        antirisk <- swap.with[[2]]
        swap.with <- swap.with[[1]]
        
        n_select <- length(select.ih)
        n_swap.with <- length(swap.with)
        
        if(n_select>0&n_swap.with>0){
          swap.with <- resample(swap.with,min(n_select,n_swap.with),prob=antirisk)
          select.ih <- select.ih[1:length(swap.with)]
          select.geo <- dat[select.ih,mget(hierarchy)]
          swap.with.geo <- dat[swap.with,mget(hierarchy)]
          dat[select.ih,c(hierarchy):=swap.with.geo]
          dat[swap.with,c(hierarchy):=select.geo]
          dat[c(select.ih,swap.with),SELECTED:=1]
          dat[,drawN:=pmax(0,drawN-sum(SELECTED)),by=c(hierarchy,"hsize")]
        }
      }
    }else{
      # finale stage
      for(h in 1:nrow(hier.i)){
        select.ih <- dat[hier.i[h],on=colnames(hier.i)][SELECTED==0,if(.N>0){resample(ID,max(drawN[1],sum(level==nhier)),prob=risk)}else{NULL}]
        if(length(select.ih)>0){
          swap.with <- dat[!hier.i[h],on=colnames(hier.i)[1:i]][SELECTED==0,.(ID,antirisk)]
          antirisk <- swap.with[[2]]
          swap.with <- swap.with[[1]]
          
          n_select <- length(select.ih)
          n_swap.with <- length(swap.with)
          
          if(n_select>0&n_swap.with>0){
            swap.with <- resample(swap.with,min(n_select,n_swap.with),prob=antirisk)
            select.ih <- select.ih[1:length(swap.with)]
            select.geo <- dat[select.ih,mget(hierarchy)]
            swap.with.geo <- dat[swap.with,mget(hierarchy)]
            dat[select.ih,c(hierarchy):=swap.with.geo]
            dat[swap.with,c(hierarchy):=select.geo]
            dat[c(select.ih,swap.with),SELECTED:=1]
            dat[,drawN:=pmax(0,drawN-sum(SELECTED)),by=c(hierarchy,"hsize")]
          }
        }
      }
    }
  }
  return(dat)
}
###

npop <- c(1,2^(1:5))*1e4
npop <- c(1000,npop,500000)
hier.types <- 2:4

mb_all <- list()
for(n in npop){
  for(h in hier.types){
    dat <- create.dat(n)
    
    hierarchy <- c("nuts1","nuts2","nuts3","nuts4")
    hierarchy <- hierarchy[1:h]
    risk <- c("ageGroup","geschl","hsize","national")
    swap <- .05 # runif(1)
    hid <- "hid"
    th <- 3
    nhier <- length(hierarchy)
    
    levels <- setLevels(dat,0:(nhier-1),5:8,4,th)
    prob <- setRisk(dat,0:(nhier-1),5:8,4)
    
    # prep data for R function
    dat_R <- copy(dat)
    dat_R[,level:=levels]
    dat_R[,risk:=prob[[1]]]
    dat_R[,antirisk:=prob[[2]]]
    dat_R <- unique(dat_R,by="hid")
    
    for(i in 1:length(hierarchy)){
      if(i==1){
        dat.draw <- dat_R[,.(drawN=randomRound(.N*swap)),by=c(hierarchy[1:i],"hsize")]
      }else{
        # distribute N
        # take number of risky households into account??????!!!!!!!!
        dat.draw.next <- dat_R[,.(N=as.numeric(.N)),by=c(hierarchy[1:i],"hsize")]
        dat.draw.next[,N:=N/sum(N),by=c("hsize",hierarchy[1:(i-1)])]
        dat.draw <- merge(dat.draw,dat.draw.next,by=c("hsize",hierarchy[1:(i-1)]),all.y=TRUE)
        dat.draw[,drawN:=randomRound(N*drawN),by=c(hierarchy[1:i],"hsize")]
        dat.draw[,N:=NULL]
      }
    }
    dat_R <- merge(dat_R,dat.draw,by=c(hierarchy,"hsize"))
    
    
    mb <- microbenchmark(cpp=recordSwap(dat,5,0:(nhier-1),5:8,4,th,swap,prob,levels),
                         # R=recordSwapR(copy(dat_R),hierarchy),
                         times=100)
    mb <- as.data.table(mb)
    mb[,npop:=n]
    mb[,hier.levels:=h]
    # save(mb,file=paste0("R/benchmark_",n,"_",h,".RData"))
    mb_all <- c(mb_all,list(mb))
  } 
}


mb_all <- rbindlist(mb_all)
# save(mb_all,file="R/benchmark.RData")
save(mb_all,file="R/benchmark_cpp.RData")
