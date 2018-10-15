#################################
# test recalib()
#

context("recalib()")
library(recordSwapping)

nhid <- 100000
dat <- recordSwapping:::create.dat( nhid )

k_anonymity <- 0
swaprate <- .1
similar <- list(c(5))
hier <- 0:2
risk_variables <- 5:7
hid <- 4

# test input parameter
test_that("test para - k_anonymity & swaprate",{
  
  dat$levels <- recordSwapping:::setLevels_cpp(dat,hier,risk_variables,hid,k_anonymity)
  table(dat$levels)
  
  dat_swapped <- as.data.table(recordSwap(dat,similar,hier,risk_variables,hid,k_anonymity,swaprate))
  
  setnames(dat_swapped,colnames(dat_swapped),colnames(dat))
  dat_swapped
  
  dat_comp <- unique(dat[,.(paste(geo=nuts1,nuts2,nuts3,nuts4,sep="_"),hid=hid,hsize=hsize,levels)])
  dat_comp_swapped <- unique(dat_swapped[,.(paste(geo=nuts1,nuts2,nuts3,nuts4,sep="_"),hid=hid,hsize=hsize)])
  dat_comp
  
  dat_comp_merge <- merge(dat_comp,dat_comp_swapped,by="hid")
  expect_true(nrow(dat_comp_merge[V1.x!=V1.y])== nhid *swaprate)
  
  dat_comp_merge <- dat_comp_merge[V1.x!=V1.y]
  bool_level <- as.matrix(dat_comp_merge[,tstrsplit(V1.x,"_")])!=as.matrix(dat_comp_merge[,tstrsplit(V1.y,"_")])
  dat_comp_merge$levels_new <- apply(bool_level,1,which.max)-1
  expect_true(nrow(dat_comp_merge[levels_new>levels])==0)
  
  k_anonymity <- 1
  dat$levels <- recordSwapping:::setLevels_cpp(dat,hier,risk_variables,hid,k_anonymity)
  table(dat$levels)

  dat_swapped <- as.data.table(recordSwap(dat,similar,hier,risk_variables,hid,k_anonymity,swaprate))
  
  setnames(dat_swapped,colnames(dat_swapped),colnames(dat))
  dat_swapped
  
  dat_comp <- unique(dat[,.(paste(geo=nuts1,nuts2,nuts3,nuts4,sep="_"),hid=hid,hsize=hsize,levels)])
  dat_comp_swapped <- unique(dat_swapped[,.(paste(geo=nuts1,nuts2,nuts3,nuts4,sep="_"),hid=hid,hsize=hsize)])
  dat_comp
  
  dat_comp_merge <- merge(dat_comp,dat_comp_swapped,by="hid")
  expect_true(nrow(dat_comp_merge[levels<length(hier)&V1.x==V1.y])==0)
  
})


test_that("test para - different hierarchies",{
  
  hierarchy <- 0:3 
  
  for(h in 1:length(hierarchy)){
    hier <- hierarchy[1:h]
    dat_swapped <- as.data.table(recordSwap(dat,similar,hier,risk_variables,hid,k_anonymity,swaprate))
    setnames(dat_swapped,colnames(dat_swapped),colnames(dat))
    dat_comp <- unique(dat[,.(paste(geo=nuts1,nuts2,nuts3,nuts4,sep="_"),hid=hid,hsize=hsize,levels)])
    dat_comp_swapped <- unique(dat_swapped[,.(paste(geo=nuts1,nuts2,nuts3,nuts4,sep="_"),hid=hid,hsize=hsize)])
    dat_comp_merge <- merge(dat_comp,dat_comp_swapped,by="hid")
    
    dat_comp_merge <- dat_comp_merge[V1.x!=V1.y]
    bool_level <- as.matrix(dat_comp_merge[,tstrsplit(V1.x,"_")])!=as.matrix(dat_comp_merge[,tstrsplit(V1.y,"_")])
    dat_comp_merge$levels_new <- apply(bool_level,1,which.max)-1
    
    expect_true(nrow(dat_comp_merge[levels_new>length(hier)])==0)
  }
  
})


test_that("test para - similar",{
  
  
})


