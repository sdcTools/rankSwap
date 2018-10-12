#################################
# test recalib()
#

context("recalib()")
library(recordSwapping)

dat <- recordSwapping:::create.dat(250000)

# test input parameter
test_that("test para - th & swaprate",{

  th <- 3
  swaprate <- .05
  similar <- list(c(5))
  hier <- 0:2
  risk <- 5:7
  hid <- 4
  
  dat_swapped <- as.data.table(recordSwap(dat,sim,hier,risk,hid,th,swaprate))
  
  setnames(dat_swapped,colnames(dat_swapped),colnames(dat))
  dat_swapped
  
  dat_comp <- unique(dat[,.(paste(geo=nuts1,nuts2,nuts3,nuts4,sep="_"),hid=hid)])
  dat_comp_swapped <- unique(dat_swapped[,.(paste(geo=nuts1,nuts2,nuts3,nuts4,sep="_"),hid=hid)])
  dat_comp
  
  dat_comp_merge <- merge(dat_comp,dat_comp_swapped,by="hid")
  dat_comp_merge[V1.x!=V1.y]
  
  
})



test_that("test para - different hierarchies",{
  
  
})


dat <- recordSwapping:::create.dat(250000)

test_that("test para - similar",{
  
  
  th <- 0
  swaprate <- .05
  similar <- list(c(5))
  hier <- 0:2
  risk <- 5:7
  hid <- 4
  
  dat_swapped <- as.data.table(recordSwap(dat,sim,hier,risk,hid,th,swaprate))
  
})


