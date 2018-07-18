##########################################################
# Example-File for record swapping
#

library(data.table)
library(Rcpp)

source("R/create_dat.R")

sourceCpp("src/recordSwap_R.cpp")

# create data
set.seed(123456)
dat <- create.dat(50000)

# apply record swapping
dat_swap <- as.data.table(recordSwap(dat,5,0:3,5:8,4,3,.1))
setnames(dat_swap,colnames(dat_swap),colnames(dat))



