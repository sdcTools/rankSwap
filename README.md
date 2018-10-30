![Build Status](https://travis-ci.org/sdcTools/recordSwapping.svg?branch=master)
[![Coverage Status](https://coveralls.io/repos/github/sdcTools/recordSwapping/badge.svg?branch=master)](https://coveralls.io/github/sdcTools/sdcMicro?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/recordSwapping)](https://CRAN.R-project.org/package=recordSwapping)
[![Downloads](http://cranlogs.r-pkg.org/badges/recordSwapping)](https://CRAN.R-project.org/package=recordSwapping)
<!--[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)-->

# recordSwapping


**recordSwapping** is an R-package for record swapping.


*src/recordSwap* contains the pure `C++` (11) code `recordSwap.cpp` and `recordSwap.h`.
In this package this code is called by an `Rcpp` wrapper which again is called from the top level `R`-function `recordSwap()`.
The `C++` code is not directly embedded using `Rcpp` so that it can be more easily implemented into other projects which do not depend on `R` libraries.
Using an additional top level `R`-functions as well as an `Rcpp` wrapper makes it more conventient to to call the pure `C++` code at the bottom as there is no mapping to every stl container from `R`.

## Versions

Information on the different versions can be found [here](https://github.com/sdcTools/recordSwapping/blob/Improvements/NEWS.md)

## Installation

The package can be installed through
```r
library(devtools)
install_github("sdcTools/recordSwapping")
```

## Application

The procedure can be applied as follows

```r
library(recordSwapping)

# create some dummy data (~ 100k households)
dat <- recordSwapping:::create.dat(100000)

# define paramters - in C++ indexing starts with 0 (!)
hierarchy <- 0:2 # nuts1 - nuts3
risk_variables <- 5:7 # hsize - gender 
hid <- 4 # column for hid

swaprate <- .05 # swaprate of households

# households are set to risky and are mandatorilly swapped
# if at least one individual has counts (over risk_variables and hierarhies)
# smaller eqaul k_anonymity
# set k_anonymity <- 0 to deactivate this feature
k_anonymity <- 3

# first similarity profile: hsize, htype, hincome
# second similarity profile: hsize
similar <- list(c(5,9,10),c(5))

# call recodSwap()
dat_swapped <- recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)
dat_swapped
```




