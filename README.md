![Build Status](https://travis-ci.org/sdcTools/recordSwapping.svg?branch=master)
[![Coverage Status](https://coveralls.io/repos/github/sdcTools/recordSwapping/badge.svg?branch=master)](https://coveralls.io/github/sdcTools/sdcMicro?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/recordSwapping)](https://CRAN.R-project.org/package=recordSwapping)
[![Downloads](http://cranlogs.r-pkg.org/badges/recordSwapping)](https://CRAN.R-project.org/package=recordSwapping)
<!--[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)-->

recordSwapping
========

**recordSwapping** is an R-package for record swapping.


*src/recordSwap* contaings the pure `C++` (11) code `recordSwap.cpp` and `recordSwap.h`.
In this package this code is called by an `Rcpp` wrapper which again is called from the top level `R`-function `recordSwap()`.
The `C++` code is not directly embedded using `Rcpp` so that it can be more easily implemented into other projects which do not depend on `R` libraries.
Using an additional top level `R`-functions as well as an `Rcpp` wrapper makes it more conventient to to call the pure `C++` code at the bottom as there is no mapping to every stl container from `R`.