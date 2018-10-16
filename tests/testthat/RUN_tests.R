library(wrswoR)
library(microbenchmark)
library(recordSwapping)
##########################################################
# Test-File for record swapping
# tests for all functions in recordSwap.cpp
#

B <- 100

# test orderData_cpp()
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


# test setRisk_cpp()
setRiskTRUE <- replicate(B,{
  dat <- recordSwapping:::create.dat(100000)
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
})
table(setRiskTRUE)


# test setLevels_cpp()
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


# test sampling function
n <- 100000
N <- 1000
Nused <- 1000
b <- 1e4
ID <- 1:n
seed.base <- 1:1e6
IDused <- rep(0,n)
IDused[sample(ID,Nused)] <- 1

prob <- sample(c(rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20)))

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

save(out_data,file="sampling.RData")
library(ggplot2)
ggplot(out_data,aes(value))+
  geom_density(aes(color=variable))
