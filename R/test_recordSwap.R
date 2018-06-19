#################################################
#
# test recordSwapping
#

library(data.table)
library(Rcpp)
set.seed(1234)

source("R/create_dat.R")

dat <- create.dat(1000000)

sourceCpp("diverses/recordSwap.cpp")


# Test c++ functions
setLevels(dat,0:2,3,0:2,1,3)


hierarchy <- c("nuts1","nuts2","nuts3")
th <- 3
dat[,level:=0]
for(i in 1:3){
  dat[level==0,N:=.N,by=c(hierarchy[1:i],"hsize")]
  dat[level==0,level:=as.numeric(i*(N<=th))]
}
dat[level==0,level:=4]

dat[,test:=setLevels(dat,0:2,3,0:2,1,3)+1]

dat[1:100,.N,by=list(nuts1,nuts2,nuts3,hsize)]

# test if outcome is same
test.fun <- function(){
  dat <- create.dat()
  hierarchy <- c("nuts1","nuts2","nuts3")
  th <- 3
  dat[,level:=0]
  for(i in 1:3){
    dat[level==0,N:=.N,by=c(hierarchy[1:i],"hsize")]
    dat[level==0,level:=as.numeric(i*(N<=th))]
  }
  dat[level==0,level:=4]
  
  dat[,test:=setLevels(dat,0:2,3,0:2,1,3)+1]
  
  if(nrow(dat[test!=level])>0){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

a <- replicate(1000,test.fun())

dat <- create.dat(1000000)
library(microbenchmark)
microbenchmark(cpp=setLevels(dat,0:2,3,0:2,1,3),
               R={dat[,level:=0];
                 for(i in 1:3){
                   dat[level==0,N:=.N,by=c(hierarchy[1:i],"hsize")]
                   dat[level==0,level:=as.numeric(i*(N<=th))]
                 }})


#
# test ordering
#
sourceCpp("diverses/recordSwap.cpp")

test <- replicate(1000,{
  dat <- create.dat()
  dat_orig <- copy(dat)
  dat <- dat[sample(1:.N)]
  dat <- orderData(dat,4)
  dat <- as.data.table(dat)
  setnames(dat,colnames(dat),colnames(dat_orig))
  all.equal(dat,dat_orig)
})

table(test)


dat <- create.dat()
dat_orig <- copy(dat)
dat <- dat[sample(1:.N)]
orderData(dat,4)


microbenchmark(orderData(dat,4),dat[order(hid)])

sourceCpp("src/recordSwap.cpp")
dat <- create.dat()
dat <- dat[sample(1:.N)]
a <- setLevels(dat,0:2,3,0:2,4,3)
as.data.table(a)


# test sampling function
sourceCpp("src/recordSwap.cpp")
library(wrswoR)
n <- 10000
ID <- 1:n
prob <- c(rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20))
N <- 100

a <- randSample(ID,N,prob)
b <- sample_int_crank(n,N,prob)

b <- sample(ID,N,prob=prob)
microbenchmark(cpp=randSample(ID,N,prob),R=sample(ID,N,prob=prob),
               sample_int_crank(n,N,prob))
microbenchmark(cpp=randSample(ID,N,prob),
               sample_int_crank(n,N,prob))
# simulate and compare with sample
# compare only distribution
sourceCpp("src/recordSwap.cpp")
n <- 100000
N <- 1000
b <- 1e4
ID <- 1:n
seed.base <- 1:1e6

prob <- sample(c(rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20)))
out_mat <- replicate(b,{
  # cpp_samp <- randSample(1:n,N,prob,seed=sample(seed.base,1))
  R_samp <- sample(1:n,N,prob=prob)
  wrswoR_samp <- sample_int_crank(n,N,prob)
  cbind(#cpp_samp,
    R_samp,wrswoR_samp)
})
out_cpp <- test_randSample(b,1:n,N,prob,seed=1)

microbenchmark(cpp=test_randSample(1,1:n,N,prob,seed=1),
               sample_int_crank(n,N,prob),R=sample(ID,N,prob=prob))

out_data <- list()
for(i in 1:b){
  out_tmp <- c(out_mat[,1:2,i],out_cpp[[i]])
  out_tmp <- data.table(value=out_tmp,run=i)
  out_tmp[,method:=rep(c("R_samp","wrswoR_samp","cpp_samp"),each=N)]
  out_data <- c(out_data,list(out_tmp))
}

out_data <- rbindlist(out_data)

# out_data_count <- out_data[,.N,by=list(value,method)]

library(ggplot2)
ggplot(out_data,aes(value))+
  geom_density(aes(color=method))


sourceCpp("src/recordSwap.cpp")
n <- 20
ID <- 1:n
prob <- c(1/rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20))
N <- 5
randSample(ID,N,prob)

prob/rexp(n)


#
# test risk calculations
#
sourceCpp("src/recordSwap.cpp")
dat <- create.dat(1000000)
hierarchy <- c("nuts1","nuts2","nuts3")
risk <- c("ageGroup","geschl","hsize","national")

dat[,risk:=1/.N,by=c(hierarchy,risk)]
dat[,risk:=max(risk),by=hid]
dat[,antirisk:=1-risk,by=hid]
dat[antirisk==0,antirisk:=5e-10,by=hid] # small probability

prob.risk <- setRisk(dat,0:2,4:7,3)
prob.risk <- as.data.table(prob.risk)


dat[,cpprisk:=prob.risk$V1]
dat[,cppantirisk:=prob.risk$V2]
dat[abs(antirisk-cppantirisk)>1e-10]

dat[,N:=.N,by=c(hierarchy,risk)]



#
# test record swapping
#
library(data.table)
library(Rcpp)

source("R/create_dat.R")

sourceCpp("src/recordSwap.cpp")
set.seed(123456)
dat <- create.dat(50000)
hierarchy <- c("nuts1","nuts2","nuts3")
risk <- c("ageGroup","geschl","hsize","national")
similar <- c("hsize")

levels <- setLevels(dat,0:3,5:8,4,3)
table(levels)
prob <- setRisk(dat,0:3,5:8,4)
dat[,levels:=levels]
dat[levels==1,.N,by=nuts1]

sourceCpp("src/recordSwap.cpp")
a <- recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels)
a
length(dat[!duplicated(hid)&nuts1==1,which=TRUE]-1)
length(sort(a))

length(unlist(a))
library(microbenchmark)
microbenchmark(recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels))

b <- recordSwap(dat,5,0:3,5:7,4,3,.1,prob,levels)

c <- c(a,b)
dat[!hid%in%dat[a+1]$hid][!duplicated(hid)][nuts1==1&nuts2==1&nuts3==1]
dat[!hid%in%dat[c+1]$hid][nuts1==1&nuts2==1&nuts3==1&!duplicated(hid)]
dat[d==0][nuts1==1&nuts2==1&nuts3==1&!duplicated(hid)]

g <- dat[hid.first&nuts1==1&nuts2==1&nuts3==1,which=TRUE]-1
g <- g[!g%in%a]
g <- g[!g%in%b]

h <- recordSwap(dat,5,0:3,5:7,4,3,.1,prob,levels)
sort(a)
b <- dat[nuts1==1&nuts2==1&nuts3==1&levels==2&!duplicated(hid),which=TRUE]-1

c <- b[!b%in%a] 

d <- recordSwap(dat,5,0:3,5:7,4,3,.1,prob,levels)
c[!c%in%d]


dat[,hid.first:=c(1,rep(0,.N-1)),by=hid]
all((dat[hid.first&levels==1,which=TRUE]-1)%in%a)
which(!(dat[hid.first&levels==1,which=TRUE]-1)%in%a)

c <- replicate(1000,recordSwap(dat,5,0:3,5:7,4,3,.1,prob,levels))


check <- table(unlist(a))
check[check>1]

dat[,hid.first:=c(1,rep(0,.N-1)),by=hid]
all((dat[hid.first&levels==0,which=TRUE]-1)%in%unlist(a))
which(!((dat[hid.first&levels<=2,which=TRUE]-1)%in%unlist(a)))
all((dat[hid.first&levels<2,which=TRUE]-1)%in%unlist(a))




all(dat[hid.first&levels==0&nuts1==1,which=TRUE]-1==sort(a))

all(sort(dat[hid.first==1&nuts1!=1,which=TRUE]-1)==sort(a))

all(sort(dat[hid.first==1&nuts1==1,which=TRUE]-1)==sort(a))
all((dat[levels==0&hid.first==1,which=TRUE]-1)==a)

nrow(dat[,.N,by=list(nuts1,nuts2,nuts3)])==length(recordSwap(dat,4,0:2,4:7,3,3,.1,prob,levels))

table(recordSwap(dat,4,0:2,4:7,3,3,.1,prob,levels))
dat[,.N,by=list(nuts1,nuts2,nuts3)][order(nuts2)][,table(nuts2)]

table(recordSwap(dat,4,0:2,4:7,3,3,.1,prob,levels))
dat[,.N,by=list(nuts1,nuts2,nuts3)][,table(N)]

dat[,.N,by=nuts1]
recordSwap(dat,4,0:2,4:7,3,3,.1,prob,levels)


#################################
# compare R and c++ Version 
# make various benchmarks
#

library(data.table)
library(Rcpp)
library(microbenchmark)
set.seed(1234)

source("R/create_dat.R")
sourceCpp("src/recordSwap.cpp")

set.seed(123456)
dat <- create.dat(500000)

levels <- setLevels(dat,0:3,5:8,4,3)
table(levels)
prob <- setRisk(dat,0:3,5:8,4)

t <- Sys.time()
a <- recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels)
Sys.time()-t
b <- recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels)

dat_R <- copy(dat)
dat_R[,level:=levels]
dat_R[,risk:=prob[[1]]]
dat_R[,antirisk:=prob[[2]]]
dat_R <- unique(dat_R,by="hid")
hierarchy <- c("nuts1","nuts2","nuts3","nuts4")
swap <- .1
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
t <- Sys.time()
recordSwapR(copy(dat_R),hierarchy)
Sys.time()-t


microbenchmark(recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels))

microbenchmark(cpp=recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels),
               R=recordSwapR(copy(dat_R),hierarchy))



npop <- c(1,2^(1:7))*1e4
npop <- c(1000,npop)
hier.types <- 2:4

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
                       R=recordSwapR(copy(dat_R),hierarchy))
  } 
}



library(ggplot2)
library(data.table)
npop <- c(1,2^(1:7))*1e4
npop <- c(1000,npop)
hier.types <- 2:4
bm_data <- list()
for(i in npop){
  for(j in hier.types){
    if(file.exists(paste0("R/benchmark_",i,"_",j,".RData"))){
      load(paste0("R/benchmark_",i,"_",j,".RData"))
      bm_data <- c(bm_data,list(mb))
    }
  }
}

bm_data <- rbindlist(bm_data)


bm_data <- bm_data[,.(value=median(time/1e9)),by=list(hier.levels,expr,npop)]
p1 <- ggplot(bm_data,aes(npop,value))+
  geom_point(aes(color=expr))+ylab("seconds")+
  facet_grid(hier.levels~.,scales = "free")
plot(p1)



library(Rcpp)
library(microbenchmark)
sourceCpp("src/recordSwap.cpp")

vec1 <- 1:1000000

test_stuff(vec1)
microbenchmark(vec1)



