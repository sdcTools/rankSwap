#######################################################
# dummy R-Code which will be exported to C++
#
#

#
# lies frame ein (vorerst....)
# bearbeite Frame auch mit simPop (-> still to do...)
options(java.parameters = "-Xmx20000m")#vor dem Laden der Pakete!!!
library(data.table)
library(sampSTAT)

dat.frame <- getFrame(ageMin=16,ageMax=60,sample.date="2017-12-01")

dat.frame <- dat.frame[,.(ID,HHID_ENCODE,GESCHL,ALTER=alter,FAMST,GEBSTAAT,STAATB,GCD,GKZ,BDL,POL,NUTS3,EDU_HAB_NAT,EINKOMMEN_EST,EINKOMMEN_LZ,ERW_STATUS,EC_DEGURBA,HH_SIZE,HH_STATUS,VERWERB,ZUZUG_OE)]

library(mountSTAT)
pfad_meth <- mountWinShare("DatenREG","REG_METHODIK","meth",verbose=FALSE)[1]
fwrite(dat.frame,paste0(pfad_meth,"/Gussenbauer/rankSwap/test.frame.csv"))


dat.frame <- fread(paste0(pfad_meth,"/Gussenbauer/rankSwap/test.frame.csv"))
dat.frame[,NUTS1:=substr(NUTS3,1,3)]
dat.frame[,NUTS2:=substr(NUTS3,4,4)]
dat.frame[,NUTS3:=substr(NUTS3,5,5)]
dat.frame <- dat.frame[HH_SIZE<=8]
dat.frame[,NUTS1:=as.numeric(factor(NUTS1))]
dat.frame[,NUTS2:=as.numeric(factor(NUTS2))]
dat.frame[,NUTS3:=as.numeric(factor(NUTS3))]
dat.frame[,AGEGROUP:=cut(ALTER,c(-Inf,20,40,60,Inf),right = FALSE)]

##############################################
# TARGET RANK SWAP
#

hierarchy <- c("NUTS1","NUTS2","NUTS3")
risk <- c("AGEGROUP","GESCHL","HH_SIZE","GEBSTAAT","ERW_STATUS")
swap <- .05 # runif(1)
seed <- 123456
hid <- "HHID_ENCODE"
th <- 3

# set swap levels for households
dat.frame[,level:=0]
for( i in 1:length(hierarchy)){
  dat.frame[level==0,N_level:=.N,by=c(hierarchy[1:i],risk)]
  dat.frame[level==0,level:=as.numeric(i*(N_level<=th))]
  dat.frame[,level:=if(any(level>0)){min(level[level>0])}else{0},by=HHID_ENCODE]
}
# dat.frame[level==0,level:=3]
# estimate risk by counts
dat.frame[,.N,by=list(N_level<=th,level)]

dat.frame[,risk:=1/.N,by=c(hierarchy,risk)]
dat.frame[,risk:=max(risk),by=HHID_ENCODE]
dat.frame[,antirisk:=1-risk,by=HHID_ENCODE]
dat.frame[antirisk==0,antirisk:=1e-10,by=HHID_ENCODE] # small probability

dat <- unique(dat.frame,by="HHID_ENCODE")


# select swap % of households by region
# loop over regions

# set drawN for highest region
# then distribute it among smaller regions

for(i in 1:length(hierarchy)){
  if(i==1){
    dat.draw <- dat[,.(drawN=randomRound(.N*swap)),by=c(hierarchy[1:i],"HH_SIZE")]
  }else{
    # distribute N
    # take number of risky households into account??????!!!!!!!!
    dat.draw.next <- dat[,.(N=as.numeric(.N)),by=c(hierarchy[1:i],"HH_SIZE")]
    dat.draw.next[,N:=N/sum(N),by=c("HH_SIZE",hierarchy[1:(i-1)])]
    dat.draw <- merge(dat.draw,dat.draw.next,by=c("HH_SIZE",hierarchy[1:(i-1)]),all.y=TRUE)
    dat.draw[,drawN:=randomRound(N*drawN),by=c(hierarchy[1:i],"HH_SIZE")]
    dat.draw[,N:=NULL]
  }
}

dat <- merge(dat,dat.draw,by=c(hierarchy,"HH_SIZE"))

## loop over hierarchies
# permute all risky households in each hierarchy level
# in last level permute remaining number of needed households (if still > 0)
dat[,SELECTED:=0]
dat[,ID:=.I]
for( i in 1:length(hierarchy)){
  hier.i <- unique(dat[,c(hierarchy[1:i],"HH_SIZE"),with=FALSE])
  # setkeyv(dat,c(hierarchy[1:i],"HH_SIZE"))
  
  if(i<length(hierarchy)){
    # upper stages
    for(h in 1:nrow(hier.i)){
      select.ih <- dat[hier.i[h],on=colnames(hier.i)][level==i&SELECTED==0,ID]
      swap.with <- dat[!hier.i[h],on=colnames(hier.i)[1:i]][HH_SIZE==hier.i[h]$HH_SIZE&SELECTED==0,resample(ID,length(select.ih),prob=antirisk)]
      select.geo <- dat[select.ih,.(NUTS1,NUTS2,NUTS3)]
      swap.with.geo <- dat[swap.with,.(NUTS1,NUTS2,NUTS3)]
      dat[select.ih,c(hierarchy):=swap.with.geo]
      dat[swap.with,c(hierarchy):=select.geo]
      dat[c(select.ih,swap.with),SELECTED:=1]
      dat[,drawN:=pmax(0,drawN-sum(SELECTED)),by=c(hierarchy,"HH_SIZE")]
    }
  }else{
    # finale stage
    for(h in 1:nrow(hier.i)){
      select.ih <- dat[hier.i[h],on=colnames(hier.i)][SELECTED==0,resample(ID,max(drawN[1],sum(level==3)),prob=risk)]
      if(length(select.ih)>0){
        swap.with <- dat[!hier.i[h],on=colnames(hier.i)[1:i]][HH_SIZE==hier.i[h]$HH_SIZE&SELECTED==0,resample(ID,length(select.ih),prob=antirisk)]
        select.geo <- dat[select.ih,.(NUTS1,NUTS2,NUTS3)]
        swap.with.geo <- dat[swap.with,.(NUTS1,NUTS2,NUTS3)]
        dat[select.ih,c(hierarchy):=swap.with.geo]
        dat[swap.with,c(hierarchy):=select.geo]
        dat[c(select.ih,swap.with),SELECTED:=1]
        dat[,drawN:=pmax(0,drawN-sum(SELECTED)),by=c(hierarchy,"HH_SIZE")]
      }
    }
  }
}

dat[,.N,by=list(SELECTED,level)]


resample <- function(x, ...){
  x[sample.int(length(x), ...)]
}

randomRound <- function(x){
  if(length(x)==1){
    if(x<1){
      return(ceiling(x))
    }else{
      if(sample(c(0,1),1)==1){
        return(ceiling(x))
      }else{
        return(floor(x))
      }
    }
  }else{
    
    x_diff <- sum(x)-floor(x)
    up_down <- rep(FALSE,length(x))
    up_down[1:x_diff] <- TRUE
    
    up_down <- sample(up_down)
    x[up_down] <- ceiling(x[up_down])
    x[!up_down] <- floor(x[!up_down])
    return(x)
  }

}


# test cpp functions
library(mountSTAT)
library(data.table)
pfad_meth <- mountWinShare("DatenREG","REG_METHODIK","meth",verbose=FALSE)[1]

dat.frame <- fread(paste0(pfad_meth,"/Gussenbauer/rankSwap/test.frame.csv"))
dat.frame[,NUTS1:=substr(NUTS3,1,3)]
dat.frame[,NUTS2:=substr(NUTS3,4,4)]
dat.frame[,NUTS3:=substr(NUTS3,5,5)]
dat.frame <- dat.frame[HH_SIZE<=8]
dat.frame[,NUTS1:=as.numeric(factor(NUTS1))]
dat.frame[,NUTS2:=as.numeric(factor(NUTS2))]
dat.frame[,NUTS3:=as.numeric(factor(NUTS3))]
dat.frame[,AGEGROUP:=cut(ALTER,c(-Inf,20,40,60,Inf),right = FALSE)]

library(Rcpp)
sourceCpp("src/recordSwap.cpp")

# c++ code index beginnt mit 0
a <- setLevels(dat.frame[1:10,.(NUTS1,NUTS2,NUTS3,HH_SIZE)],c(0:2),3,c(0:2),1,3)

dat.frame[1:10,.(NUTS1,NUTS2,NUTS3,HH_SIZE)][,.N,by=list(NUTS1,NUTS2,NUTS3,HH_SIZE)]

microbenchmark(setLevels(dat.frame[1:10,.(NUTS1,NUTS2,NUTS3,HH_SIZE)],c(0:2),3,c(0:2),1,3),
               dat.frame[1:10,.(NUTS1,NUTS2,NUTS3,HH_SIZE)][,.N,by=list(NUTS1,NUTS2,NUTS3,HH_SIZE)])


library(microbenchmark)

microbenchmark(setLevels(dat.frame[,.(NUTS1,NUTS2,NUTS3,HH_SIZE)],c(0:2),3,c(0:2),1,3))

microbenchmark(dat.frame[,.(NUTS1,NUTS2,NUTS3,HH_SIZE)][,as.character(HH_SIZE)])
