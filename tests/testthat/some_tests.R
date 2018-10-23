##########################################################
# Test-File for record swapping
# tests for all functions in recordSwap.cpp
#

library(wrswoR)
library(microbenchmark)
library(recordSwapping)


############################
# test order data
# data will be ordered by hid per default
B <- 100
orderTRUE <- replicate(B,{
  dat <- recordSwapping:::create.dat(100000)
  
  dat_order <- dat[sample(.N)]
  dat_transpose <- transpose(dat_order)
  dat_order <- as.data.table(recordSwapping:::orderData_cpp(dat_transpose,4))
  dat_order <- transpose(dat_order)
  setnames(dat_order,colnames(dat_order),colnames(dat))
  all.equal(dat$hid,dat_order$hid)
})

table(orderTRUE)

############################
# test setRisk
# if not supplied by user this can be done by using k anonymity
# risk will be setup
# then sampling probability ~ risk
dat <- recordSwapping:::create.dat(50000)
dat_t <- transpose(dat)
hierarchy <- 0:3
risk_variables <- 5:8
hid <- 4
risk <- recordSwapping:::setRisk_cpp(dat_t,hierarchy,risk_variables,hid)
risk <- as.data.table(transpose(risk))
risk <- cbind(hid=dat$hid,risk)

risk_direct <- dat[,.(hid)]
for(i in 1:length(hierarchy)){
  by.cols <- colnames(dat)[c(risk_variables,hierarchy)+1]
  by.cols <- by.cols[1:(length(risk_variables)+i)]
  risk_i <- dat[,.(hid,1/.N),by=c(by.cols)]
  risk_i <- unique(risk_i[,.(max(V2)),by=hid])
  setnames(risk_i,"V1",paste0("V",i))
  risk_direct <- merge(risk_direct,risk_i)
}
all.equal(risk_direct,risk,check.attributes=FALSE)

microbenchmark(cpp={
  risk <- recordSwapping:::setRisk_cpp(dat_t,hierarchy,risk_variables,hid)
#  risk <- as.data.table(transpose(risk))
#  risk <- cbind(hid=dat$hid,risk)
},
R={
  risk_direct <- dat[,.(hid)]
  for(i in 1:length(hierarchy)){
    by.cols <- colnames(dat)[c(risk_variables,hierarchy)+1]
    by.cols <- by.cols[1:(length(risk_variables)+i)]
    risk_i <- dat[,.(hid,1/.N),by=c(by.cols)]
    risk_i <- unique(risk_i[,.(max(V2)),by=hid])
    setnames(risk_i,"V1",paste0("V",i))
    risk_direct <- merge(risk_direct,risk_i)
  }
})
############################
# test setLevels
# set swap levels for person which does not fullfill risk threshold
# again this is either supplied by the user with risk_threshold or k_anonymity
#
dat <- recordSwapping:::create.dat(100000)
dat_t <- transpose(dat)
hierarchy <- 0:3
risk_variables <- 5:8
hid <- 4
risk <- recordSwapping:::setRisk_cpp(dat_t,hierarchy,risk_variables,hid)
risk_threshold <- 1/3
level_cpp <- recordSwapping:::setLevels_cpp(risk,risk_threshold)


hierarchy <- c("nuts1","nuts2","nuts3","nuts4")
risk <- c("hsize","ageGroup","gender","national")
th <- 3
dat[,level:=0]
for(i in 1:4){
  dat[level==0,N:=.N,by=c(hierarchy[1:i],risk)]
  dat[level==0,level:=as.numeric(i*(N<=th))]
}
dat[level==0,level:=5]
dat[,level:=min(level),by=hid]
dat <- dat[order(hid)]

all((dat$level-1)==level_cpp)

############################
# test sampling procedure
# test this also for partial sampling e.g. if some units have already been
# drawn skipping thos in the procedure should result in the same density function
#
#
n <- 100000
N <- 1000
Nused <- 1000
b <- 1e4
ID <- 1:n
seed.base <- 1:1e6
IDused <- rep(0,n)
startpoint <- sample(ID[1:(max(ID)-Nused)],1)
IDused[startpoint:(startpoint+Nused-1)] <- 1

prob <- sample(c(rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20)))

microbenchmark(cpp_samp = recordSwapping:::test_randSample_cpp(1:n,N,prob,IDused,seed=sample(seed.base,1)))

out_mat <- replicate(b,{
  cpp_samp <- recordSwapping:::test_randSample_cpp(1:n,N,prob,IDused,seed=sample(seed.base,1))
  R_samp <- sample(ID[IDused==0],N,prob=prob[IDused==0])
  wrswoR_samp <- wrswoR::sample_int_crank(n-Nused,N,prob[IDused==0])
  wrswoR_samp <- ID[IDused==0][wrswoR_samp]
  cbind(cpp_samp,
        R_samp,wrswoR_samp)
})


out_data <- list()
for(i in 1:b){
  out_tmp <- out_mat[,1:3,i]
  out_tmp <- data.table(value=out_tmp,run=i)
  out_data <- c(out_data,list(out_tmp))
}

out_data <- rbindlist(out_data)
out_data <- melt(out_data,id.vars="run")

library(ggplot2)
ggplot(out_data,aes(value))+
  geom_density(aes(color=variable))

############################
# test distribution of swaprate  
# at lowest level
# 
dat <- recordSwapping:::create.dat(100000)
dat_t <- transpose(dat)
hierarchy <- 0:3
swaprate <- .1
hid <- 4

seed.base <- 1:1e6
distdraw <- as.data.table(recordSwapping:::test_distributeDraws_cpp(dat_t,hierarchy,hid,swaprate,sample(seed.base,1)))
distdraw <- transpose(distdraw)

# compare with R solution
hier <- colnames(dat)[hierarchy+1]
distdrawR <- dat[!duplicated(hid),.(ngroup=.N),by=c(hier)]
distdrawR[,draw:=recordSwapping:::randomRound(ngroup*(swaprate/2))]

setnames(distdraw,colnames(distdraw),colnames(distdrawR))

comp_dist <- distdraw[distdrawR,,on=c(hier)]

cond1 <- nrow(comp_dist[ngroup!=i.ngroup])==0
cond2 <- comp_dist[,sum(draw)==sum(i.draw)]
cond3 <- nrow(comp_dist[abs(draw-i.draw)>1])==0

cond1&cond2&cond3

############################
# test sampling of donor
# make this more modular and more efficient
# sampling in different hierarchies should be easy when using just the std::vector<std::vector<double>> risk
# 
dat <- recordSwapping:::create.dat(10000)
dat[,hid:=.I]
dat_t <- transpose(dat)
hierarchy <- 0
risk_variables <- 5:8
hid <- 4
similar <- list(c(5))
risk <- recordSwapping:::setRisk_cpp(dat_t,hierarchy,risk_variables,hid)
dat$prob <- unlist(transpose(risk))

nsamp <- 3000
swap_pool <- sample(unique(dat$nuts1),1)
IDswap_vec <- dat[nuts1==swap_pool,sample(.I,nsamp)]-1 
IDswap_pool_vec <- dat[nuts1==swap_pool,which=TRUE]-1

sim_names <- colnames(dat)[unlist(similar)+1]
n_by_similar <- dat[IDswap_vec+1,.N,by=c(sim_names)]
dat <- n_by_similar[dat,,on=c(sim_names)]


b <- 1000
out_mat <- replicate(b,{
  seed <- sample(1:1e6,1)
  set.seed(seed)
  IDdonor <- recordSwapping:::test_sampleDonor_cpp(dat_t,similar,hid,IDswap_vec,IDswap_pool_vec, dat$prob,seed)
  IDdonor_R <- dat[nuts1!=swap_pool,sample(.I-1,N[1],prob=prob),by=c(sim_names)][,V1]
  cbind(IDdonor,
        IDdonor_R)
})
out_data <- list()
for(i in 1:b){
  out_tmp <- out_mat[,1:2,i]
  out_tmp <- data.table(value=out_tmp,run=i)
  out_data <- c(out_data,list(out_tmp))
}

out_data <- rbindlist(out_data)
out_data <- melt(out_data,id.vars="run")

library(ggplot2)
ggplot(out_data,aes(value))+
  geom_density(aes(color=variable))


############################
# check recordSwap as a whole
# 
dat <- recordSwapping:::create.dat(100000)
dat_t <- transpose(dat)
hierarchy <- 0:2
risk_variables <- 5:6
k_anonymity <- 3
swaprate <- .1
hid <- 4
similar <- list(c(5))

risk <- recordSwapping:::setRisk_cpp(dat_t,hierarchy,risk_variables,hid)
risk_threshold <- 1/k_anonymity
level_cpp <- recordSwapping:::setLevels_cpp(risk,risk_threshold)
table(level_cpp)

# run recordSwap()
dat_swapped <-recordSwap(dat_t,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)
dat_swapped <- as.data.table(dat_swapped)
dat_swapped <- transpose(dat_swapped)
setnames(dat_swapped,colnames(dat_swapped),colnames(dat))

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])


# run recordSwap() with
# k_anonymity=0
k_anonymity <- 0
dat_swapped <-recordSwap(dat_t,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)
dat_swapped <- as.data.table(dat_swapped)
dat_swapped <- transpose(dat_swapped)
setnames(dat_swapped,colnames(dat_swapped),colnames(dat))

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# run recordSwap() with 
# k_anonymity=0 and swaprate=0.2
swaprate <- .2
dat_swapped <-recordSwap(dat_t,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)
dat_swapped <- as.data.table(dat_swapped)
dat_swapped <- transpose(dat_swapped)
setnames(dat_swapped,colnames(dat_swapped),colnames(dat))

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# run recordSwap() with 
# k_anonymity=0 and swaprate=0.2
swaprate <- .2
dat_swapped <- recordSwap(dat_t,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)
dat_swapped <- as.data.table(dat_swapped)
dat_swapped <- transpose(dat_swapped)
setnames(dat_swapped,colnames(dat_swapped),colnames(dat))

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# run recordSwap() with 
# k_anonymity=0 and swaprate=0.1
# and 1 hierarchy level
hierarchy <- 0
swaprate <- 0.1
dat_swapped <- recordSwap(dat_t,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)
dat_swapped <- as.data.table(dat_swapped)
dat_swapped <- transpose(dat_swapped)
setnames(dat_swapped,colnames(dat_swapped),colnames(dat))

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])



############################
# some additional tests
N <- 10000
n <- 500
x <- sample(1:N)
x <- 1:N
mustSwap <- c(1:100)
prob <- c(rnorm(N-n,mean=5),rnorm(n,mean=30))
seed <- sample(1:1e6,1)
recordSwapping:::test_comparator(x,prob,mustSwap,n,seed)
seed <- sample(1:1e6,1)
recordSwapping:::test_prioqueue(x,prob,mustSwap,n,seed)
c <- sample(x,n,prob=prob)
a <- recordSwapping:::test_comparator(x,prob,mustSwap,n,seed)
b <- recordSwapping:::test_prioqueue(x,prob,mustSwap,n,seed)

microbenchmark(comp=recordSwapping:::test_comparator(x,prob,mustSwap,n,seed),
               queue=recordSwapping:::test_prioqueue(x,prob,mustSwap,n,seed),
               R=sample(x,n,prob=prob)
               # wrswoR = x[wrswoR::sample_int_crank(N,n,prob)]
               )

b <- 1000
out_mat <- replicate(b,{
  seed <- sample(1:1e6,1)
  comp <- recordSwapping:::test_comparator(x,prob,mustSwap,n,seed)
  queue <- recordSwapping:::test_prioqueue(x,prob,mustSwap,n,seed)
  R <- sample(x,n,prob=prob)
  wrswoR <-  x[wrswoR::sample_int_crank(N,n,prob)]
  cbind(comp,queue,R,wrswoR)
})

out_data <- list()
for(i in 1:b){
  out_tmp <- out_mat[,1:4,i]
  out_tmp <- data.table(value=out_tmp,run=i)
  out_data <- c(out_data,list(out_tmp))
}

out_data <- rbindlist(out_data)
out_data <- melt(out_data,id.vars="run")
out_data
library(ggplot2)
ggplot(out_data[variable%in%c("value.comp","value.queue")],aes(value))+
  geom_density(aes(color=variable))





####################################################################################
####################################################################################
####################################################################################
####################################################################################

dat <- recordSwapping:::create.dat(100000)

dat_order <- dat[sample(.N)]
dat_transpose <- transpose(dat_order)
dat_order <- as.data.table(recordSwapping:::orderData_cpp(dat_transpose,4))
dat_order <- transpose(dat_order)
setnames(dat_order,colnames(dat_order),colnames(dat))
all.equal(dat$hid,dat_order$hid)

# library(data.table)
# library(Rcpp)
# library(wrswoR)
set.seed(1234)

# source("R/create_dat.R")
# sourceCpp("src/recordSwap.cpp")

dat <- recordSwapping:::create.dat(100000)

###############################
# test set level function
# test if outcome is same
test.fun <- function(){
  dat <- recordSwapping:::create.dat()
  hierarchy <- c("nuts1","nuts2","nuts3","nuts4")
  risk <- c("hsize","ageGroup","geschl","national")
  th <- 3
  dat[,level:=0]
  for(i in 1:4){
    dat[level==0,N:=.N,by=c(hierarchy[1:i],risk)]
    dat[level==0,level:=as.numeric(i*(N<=th))]
  }
  dat[level==0,level:=5]
  dat[,level:=min(level),by=hid]
  
  dat[,test:=recordSwapping:::setLevels_cpp(dat,0:3,5:8,4,3)+1]
  
  if(nrow(dat[test!=level])>0){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

a <- replicate(1000,test.fun())
table(a)

###############################

###############################
# test ordering function
test <- replicate(1000,{
  dat <- create.dat()
  dat_orig <- copy(dat)
  dat <- dat[sample(1:.N)]
  dat <- recordSwapping:::orderData_cpp(dat,4)
  dat <- as.data.table(dat)
  setnames(dat,colnames(dat),colnames(dat_orig))
  all.equal(dat[,.(hid)],dat_orig[,.(hid)])
})

###############################

###############################
# test sampling function
n <- 100000
N <- 1000
b <- 1e4
ID <- 1:n
seed.base <- 1:1e6

prob <- sample(c(rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20)))
out_mat <- replicate(b,{
  cpp_samp <- test_randSample_cpp(b,1:n,N,prob,seed=1)
  R_samp <- sample(1:n,N,prob=prob)
  wrswoR_samp <- wrswoR::sample_int_crank(n,N,prob)
  cbind(cpp_samp,
    R_samp,wrswoR_samp)
})
out_cpp <- recordSwapping:::test_randSample_cpp(b,1:n,N,prob,seed=1)

out_data <- list()
for(i in 1:b){
  out_tmp <- c(out_mat[,1:2,i],out_cpp[[i]])
  out_tmp <- data.table(value=out_tmp,run=i)
  out_tmp[,method:=rep(c("R_samp","wrswoR_samp","cpp_samp"),each=N)]
  out_data <- c(out_data,list(out_tmp))
}

out_data <- rbindlist(out_data)

library(ggplot2)
ggplot(out_data,aes(value))+
  geom_density(aes(color=method))
###############################

###############################
# test swapping outcome
#
b <- 500
all_hid <- rep(FALSE,b)
all_swapped <- rep(FALSE,b)

for(i in 1:b){
  dat <- create.dat(50000)
  dat_R <- copy(dat)
  levels <- recordSwapping:::setLevels_cpp(dat,0:3,5:8,4,3)
  prob <- recordSwapping:::setRisk_cpp(dat,0:3,5:8,4)
  dat_R[,level:=levels]
  dat_R[,risk:=prob[[1]]]
  dat_R[,antirisk:=prob[[2]]]
  dat_R <- unique(dat_R,by="hid")
  hierarchy <- c("nuts1","nuts2","nuts3","nuts4")
  swap <- .1
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
  
  swap_R <- recordSwapR(copy(dat_R),hierarchy)
  swap_cpp <- as.data.table(recordSwap(dat,5,0:3,5:8,4,3,.1))
  setnames(swap_cpp,colnames(swap_cpp),colnames(dat))
  
  swap_cpp[,comb_hier:=paste(nuts1,nuts2,nuts3,nuts4)]
  if(nrow(swap_cpp[,uniqueN(comb_hier),by=hid][V1>1])>0){
    all_hid[i] <- TRUE
  }
  
  dat[,levels:=levels]
  orig_id <- unique(dat[levels<length(hierarchy)-1,.(hid,orig=paste(nuts1,nuts2,nuts3,nuts4))])
  swap_cpp[,levels:=levels]
  changed_id <- unique(swap_cpp[levels<length(hierarchy)-1,.(hid,new=paste(nuts1,nuts2,nuts3,nuts4))])
  id_was_swapped <- merge(orig_id,changed_id,by="hid")
  if(nrow(id_was_swapped[orig==new])>0){
    all_swapped[i] <- TRUE
  }
}
table(all_swapped)
table(all_hid)

library(recordSwapping)

swap_cpp <- recordSwap(dat,5,0:3,5:8,4,0,.1)

dat <- recordSwapping:::create.dat(N=500000)
levels <- recordSwapping:::setLevels_cpp(dat,0:2,5:7,4,3)
table(levels)
th <- 3
swaprate <- .05
dat_swapped <- as.data.table(recordSwap(dat,5,0:2,5:7,4,th,swaprate))
setnames(dat_swapped,colnames(dat_swapped),colnames(dat))
dat_swapped

dat_comp <- unique(dat[,.(paste(geo=nuts1,nuts2,nuts3,nuts4,sep="_"),hid=hid)])
dat_comp_swapped <- unique(dat_swapped[,.(paste(geo=nuts1,nuts2,nuts3,nuts4,sep="_"),hid=hid)])
dat_comp

dat_comp_merge <- merge(dat_comp,dat_comp_swapped,by="hid")
dat_comp_merge[V1.x!=V1.y]


set.seed(1234)


dat <- recordSwapping:::create.dat(1000000)


# Test c++ functions
recordSwapping:::setLevels_cpp(dat,0:2,3,0:2,1,3)

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



dat <- recordSwapping:::create.dat(1000000)



#
# test ordering
#


dat <- recordSwapping:::create.dat()
dat_orig <- copy(dat)
dat <- dat[sample(1:.N)]
recordSwapping:::orderData_cpp(dat,4)


microbenchmark(recordSwapping:::orderData_cpp(dat,4),dat[order(hid)])

dat <- recordSwapping:::create.dat()
dat <- dat[sample(1:.N)]
a <- recordSwapping:::setLevels_cpp(dat,0:2,3,0:2,4,3)
as.data.table(a)


# test sampling function
n <- 10000
ID <- 1:n
prob <- c(rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20))
N <- 100

a <- recordSwapping:::randSample_(ID,N,prob)
b <- wrswoR::sample_int_crank(n,N,prob)

b <- sample(ID,N,prob=prob)
microbenchmark(cpp=randSample(ID,N,prob),R=sample(ID,N,prob=prob),
               wrswoR::sample_int_crank(n,N,prob))
microbenchmark(cpp=randSample(ID,N,prob),
               wrswoR::sample_int_crank(n,N,prob))
# simulate and compare with sample
# compare only distribution
n <- 100000
N <- 1000
b <- 1e4
ID <- 1:n
seed.base <- 1:1e6

prob <- sample(c(rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20)))
out_mat <- replicate(b,{
  # cpp_samp <- randSample(1:n,N,prob,seed=sample(seed.base,1))
  R_samp <- sample(1:n,N,prob=prob)
  wrswoR_samp <- wrswoR::sample_int_crank(n,N,prob)
  cbind(#cpp_samp,
    R_samp,wrswoR_samp)
})
out_cpp <- test_randSample(b,1:n,N,prob,seed=1)

microbenchmark(cpp=test_randSample(1,1:n,N,prob,seed=1),
               wrswoR::sample_int_crank(n,N,prob),R=sample(ID,N,prob=prob))

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

n <- 20
ID <- 1:n
prob <- c(1/rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20))
N <- 5
recordSwapping:::randSample_cpp(ID,N,prob)

prob/rexp(n)


#
# test risk calculations
#
dat <- recordSwapping:::create.dat(1000000)
hierarchy <- c("nuts1","nuts2","nuts3")
risk <- c("ageGroup","geschl","hsize","national")

dat[,risk:=1/.N,by=c(hierarchy,risk)]
dat[,risk:=max(risk),by=hid]
dat[,antirisk:=1-risk,by=hid]
dat[antirisk==0,antirisk:=5e-10,by=hid] # small probability

prob.risk <- recordSwapping:::setRisk_cpp(dat,0:2,4:7,3)
prob.risk <- as.data.table(prob.risk)


dat[,cpprisk:=prob.risk$V1]
dat[,cppantirisk:=prob.risk$V2]
dat[abs(antirisk-cppantirisk)>1e-10]

dat[,N:=.N,by=c(hierarchy,risk)]



#
# test record swapping
#
set.seed(123456)
dat <- recordSwapping:::create.dat(50000)
hierarchy <- c("nuts1","nuts2","nuts3")
risk <- c("ageGroup","geschl","hsize","national")
similar <- c("hsize")

levels <- recordSwapping:::setLevels_cpp(dat,0:3,5:8,4,3)
table(levels)
prob <- recordSwapping:::setRisk_cpp(dat,0:3,5:8,4)
dat[,levels:=levels]
dat[levels==1,.N,by=nuts1]

a <- recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels)
a
length(dat[!duplicated(hid)&nuts1==1,which=TRUE]-1)
length(sort(a))

length(unlist(a))
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


set.seed(123456)
dat <- recordSwapping:::create.dat(9000000)

levels <- recordSwapping:::setLevels_cpp(dat,0:3,5:8,4,3)
table(levels)
prob <- recordSwapping:::setRisk_cpp(dat,0:3,5:8,4)

t <- Sys.time()
a <- recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels)
Sys.time()-t

# sourceCpp("src/recordSwap_old.cpp")
# t <- Sys.time()
# b <- recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels)
# Sys.time()-t

dat_R <- copy(dat)
dat_R[,level:=levels]
dat_R[,risk:=prob[[1]]]
dat_R[,antirisk:=prob[[2]]]
dat_R <- unique(dat_R,by="hid")
hierarchy <- c("nuts1","nuts2","nuts3","nuts4")
swap <- .1
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
t <- Sys.time()
recordSwapR(copy(dat_R),hierarchy)
Sys.time()-t


microbenchmark(recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels))

microbenchmark(cpp=recordSwap(dat,5,0:3,5:8,4,3,.1,prob,levels),
               R=recordSwapR(copy(dat_R),hierarchy))



npop <- c(1,2^(1:6))*1e4
npop <- c(1000,npop)
hier.types <- 2:4

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
                       R=recordSwapR(copy(dat_R),hierarchy),times=50)
  } 
}



library(ggplot2)
library(data.table)
npop <- c(1,2^(1:5))*1e4
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

load("R/benchmark.RData")
bm_data <- mb_all[,.(value=mean(time/1e9)),by=list(hier.levels,expr,npop)]
p1 <- ggplot(bm_data,aes(npop,value))+
  geom_point(aes(color=factor(expr)))+ylab("seconds")+
  facet_grid(hier.levels~.,scales="free")
plot(p1)



load("R/benchmark_cpp.RData")
bm_data <- mb_all[,.(value=mean(time/1e9)),by=list(hier.levels,expr,npop)]
p1 <- ggplot(bm_data,aes(npop,value))+
  geom_point(aes(color=factor(hier.levels)))+ylab("seconds")
plot(p1)


vec1 <- 1:1000000

test_stuff(vec1)
microbenchmark(vec1)


###################################################
# create test battery for comparison with R-Code
#
#
# test ordering
#

test <- replicate(1000,{
  dat <- recordSwapping:::create.dat()
  dat_orig <- copy(dat)
  dat <- dat[sample(1:.N)]
  dat <- recordSwapping:::orderData_cpp(dat,4)
  dat <- as.data.table(dat)
  setnames(dat,colnames(dat),colnames(dat_orig))
  all.equal(dat,dat_orig)
})

table(test)






