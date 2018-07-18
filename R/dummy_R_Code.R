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

dat <- recordSwapR(copy(dat),hierarchy)
dat[,.N,by=list(SELECTED,level)]

