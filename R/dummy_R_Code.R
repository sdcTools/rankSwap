#######################################################
# dummy R-Code which will be exported to C++
#
#

library(data.table)
library(Rcpp)
set.seed(1234)

source("R/create_dat.R")

dat <- create.dat(40000)

##############################################
# TARGET RANK SWAP
#

hierarchy <- c("nuts1","nuts2","nuts3","nuts4")
risk <- c("ageGroup","geschl","hsize","national")
swap <- .05 # runif(1)
seed <- 123456
hid <- "hid"
th <- 3

# set swap levels for households
dat[,level:=0]
for( i in 1:length(hierarchy)){
  dat[level==0,N_level:=.N,by=c(hierarchy[1:i],risk)]
  dat[level==0,level:=as.numeric(i*(N_level<=th))]
  dat[,level:=if(any(level>0)){min(level[level>0])}else{0},by=hid]
}

# dat[level==0,level:=3]
# estimate risk by counts
dat[,.N,by=list(N_level<=th,level)]

dat[,risk:=1/.N,by=c(hierarchy,risk)]
dat[,risk:=max(risk),by=hid]
dat[,antirisk:=1-risk,by=hid]
dat[antirisk==0,antirisk:=1e-10,by=hid] # small probability

dat <- unique(dat,by="hid")


# select swap % of households by region
# loop over regions

# set drawN for highest region
# then distribute it among smaller regions

for(i in 1:length(hierarchy)){
  if(i==1){
    dat.draw <- dat[,.(drawN=randomRound(.N*swap)),by=c(hierarchy[1:i],"hsize")]
  }else{
    # distribute N
    # take number of risky households into account??????!!!!!!!!
    dat.draw.next <- dat[,.(N=as.numeric(.N)),by=c(hierarchy[1:i],"hsize")]
    dat.draw.next[,N:=N/sum(N),by=c("hsize",hierarchy[1:(i-1)])]
    dat.draw <- merge(dat.draw,dat.draw.next,by=c("hsize",hierarchy[1:(i-1)]),all.y=TRUE)
    dat.draw[,drawN:=randomRound(N*drawN),by=c(hierarchy[1:i],"hsize")]
    dat.draw[,N:=NULL]
  }
}

dat <- merge(dat,dat.draw,by=c(hierarchy,"hsize"))

## loop over hierarchies
# permute all risky households in each hierarchy level
# in last level permute remaining number of needed households (if still > 0)
# create function to compare with c++ version
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

dat <- recordSwapR(copy(dat),hierarchy)
dat[,.N,by=list(SELECTED,level)]

