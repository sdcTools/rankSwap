##########################################################
# Test-File for record swapping
# tests for all functions in recordSwap.cpp
#
library(wrswoR)
library(microbenchmark)
library(recordSwapping)

B <- 100

#########
# test orderData_cpp()
orderTRUE <- replicate(B,{
  dat <- recordSwapping:::create.dat(100000)
  
  dat_order <- dat[sample(.N)]
  dat_order <- as.data.table(recordSwapping:::orderData_cpp(dat_order,4))
  setnames(dat_order,colnames(dat_order),colnames(dat))
  all.equal(dat$hid,dat_order$hid)
})

table(orderTRUE)


#########
# test setRisk_cpp()
setRiskTRUE <- replicate(B,{
  dat <- recordSwapping:::create.dat(100000)
  hierarchy <- 0:3
  risk_variables <- 5:8
  hid <- 4
  risk <- recordSwapping:::setRisk_cpp(dat,hierarchy,risk_variables,hid)
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
})
table(setRiskTRUE)


#########
# test setLevels_cpp()
setLevelsTRUE <- replicate(B,{
  dat <- recordSwapping:::create.dat(100000)
  hierarchy <- 0:3
  risk_variables <- 5:8
  hid <- 4
  risk <- recordSwapping:::setRisk_cpp(dat,hierarchy,risk_variables,hid)
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
})
table(setLevelsTRUE)


#########
# test sampling function
n <- 10000
N <- 1000
Nused <- 1000
b <- 1e3
ID <- 1:n
seed.base <- 1:1e6
IDused <- rep(0,n)

startpoint <- sample(ID[1:(max(ID)-Nused)],1)
IDused[startpoint:(startpoint+Nused-1)] <- 1

prob <- sample(c(rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20)))

out_mat <- replicate(b,{
  cpp_samp <- recordSwapping:::randSample_cpp(1:n,N,prob,IDused,seed=sample(seed.base,1))
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

save(out_data,file="sampling.RData")
library(ggplot2)
ggplot(out_data,aes(value))+
  geom_density(aes(color=variable))

ggplot(out_data,aes(value))+
  geom_histogram(aes(fill=variable),position = "dodge")

#########
# test drawDistribution()
drawDistrubtionTRUE <- replicate(B,{
  dat <- recordSwapping:::create.dat(100000)
  hierarchy <- 0:3
  swaprate <- .1
  hid <- 4
  
  seed.base <- 1:1e6
  distdraw <- as.data.table(recordSwapping:::distributeDraws_cpp(dat,hierarchy,hid,swaprate,sample(seed.base,1)))
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
})
table(drawDistrubtionTRUE)


#########
# test sampleDonor()

dat <- recordSwapping:::create.dat(10000)
dat[,hid:=.I]
hierarchy <- 0
risk_variables <- 5:8
hid <- 4
similar <- c(5)
risk <- recordSwapping:::setRisk_cpp(dat,hierarchy,risk_variables,hid)
dat$prob <- unlist(transpose(risk))

nsamp <- 300
swap_pool <- sample(unique(dat$nuts1),1)
IDswap_vec <- dat[nuts1==swap_pool,sample(.I,nsamp)]-1 
IDswap_pool_vec <- dat[nuts1==swap_pool,which=TRUE]-1

sim_names <- colnames(dat)[unlist(similar)+1]
n_by_similar <- dat[IDswap_vec+1,.N,by=c(sim_names)]
dat <- n_by_similar[dat,,on=c(sim_names)]


b <- 5000
out_mat <- replicate(b,{
  seed <- sample(1:1e6,1)
  set.seed(seed)
  IDdonor <- recordSwapping:::sampleDonor_cpp(dat,similar,hid,IDswap_vec,IDswap_pool_vec, dat$prob,seed)
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
dat <- recordSwapping:::create.dat(80000)
hierarchy <- 0:3
risk_variables <- 6:8
k_anonymity <- 1
swaprate <- .1
hid <- 4
similar <- c(5)

risk <- recordSwapping:::setRisk_cpp(dat,hierarchy,risk_variables,hid)
risk_threshold <- 1/k_anonymity
level_cpp <- recordSwapping:::setLevels_cpp(risk,risk_threshold)
table(level_cpp)

# run recordSwap()
dat_swapped <- copy(dat)
dat_swapped <- recordSwap(dat_swapped,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])


# run recordSwap() with
# k_anonymity=0
k_anonymity <- 0
dat_swapped <-recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# run recordSwap() with 
# k_anonymity=0 and swaprate=0.2
swaprate <- .2
dat_swapped <-recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# run recordSwap() with 
# k_anonymity=0 and swaprate=0.1
# and 1 hierarchy level
hierarchy <- 0
swaprate <- 0.1
dat_swapped <- recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# run recordSwap() with 
# k_anonymity=0 and swaprate=0.1
# where donor cannot be found
hierarchy <- 0:2
k_anonymity <- 0
similar <- list(c(0,5,9,10))

dat_swapped <- recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])


############################
# plot benchmarks
library(ggplot2)
load("R/benchmark_cpp.RData")
str(mb_all)
mb_all <- rbindlist(mb_all)

p1 <- ggplot(mb_all,aes(factor(npop),time))+
  geom_boxplot(aes(fill=factor(hier.levels)))
plot(p1)

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

