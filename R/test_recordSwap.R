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

sourceCpp("diverses/recordSwap.cpp")
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
b <- 1e5
ID <- 1:n
prob <- c(rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20))
out_mat <- replicate(b,{
  cpp_samp <- randSample(1:n,N,prob)
  R_samp <- sample(1:n,N,prob=prob)
  wrswoR_samp <- sample_int_crank(n,N,prob)
  cbind(cpp_samp,R_samp,wrswoR_samp)
})


out_data <- list()
for(i in 1:b){
  out_tmp <- c(out_mat[,1:3,i])
  out_tmp <- data.table(value=out_tmp,run=i)
  out_tmp[,method:=rep(c("cpp_samp","R_samp","wrswoR_samp"),each=N)]
  out_data <- c(out_data,list(out_tmp))
}

out_data <- rbindlist(out_data)

# out_data_count <- out_data[,.N,by=list(value,method)]

library(ggplot2)
p1 <- ggplot(out_data,aes(value))+
  geom_density(aes(color=method))
plot(p1)


sourceCpp("src/recordSwap.cpp")
n <- 20
ID <- 1:n
prob <- c(1/rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20))
N <- 5
randSample(ID,N,prob)

prob/rexp(n)

