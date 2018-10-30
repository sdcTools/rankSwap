# surveysd 0.2.0

* Some parameter changes to the `C++` function `recordSwap()`:

    + `similar` is now a vector of vectors allowing for multiple similarity profiles
    + changed `std::vector<int> risk` to `std::vector<int> risk_variables` for a more descriptive name
    + changed `int th` to `int k_anonymity` for a more descriptive name
    + added parameter `double risk_threshold` which can be used to set an custom risk threshold. household whith a risk **greater or equal** `risk_threshold` is set as risky household
    + added parameter `std::vector<std::vector<double>> risk` which can be used as custom risks for each household in each hierarchy level.

```r
recordSwap(std::vector< std::vector<int> > data, std::vector<std::vector<int>> similar,
                                           std::vector<int> hierarchy, std::vector<int> risk_variables, int hid, 
                                           int k_anonymity, double swaprate, double risk_threshold,
                                           std::vector<std::vector<double>> risk, int seed = 123456)
```
                                           
* Fixed some bugs concerning the application of `swaprate`
* `data` as well as `risk` are used internal such that `data[0]` or `risk[0]` contain the micro data or risk,  over all hierarchies, for the first individual,
`data[1]` or `risk[1]` contain the micro data or risk, over all hierarchies, for the second individual, and so on.
* Documentation and paramter description have been updated in the corresponding help files.

# surveysd 0.1.0

* First prototype version of record swapping 
* Contains the function `recordSwap()` as the main function of this package
* `recordSwap()` is an `R`-wrapper which calls an underlying `Rcpp` function where at the bottom a call to the **C++** function `recordSwap()` is made.

```r
recordSwap(std::vector< std::vector<int>> data, std::vector<int> similar,
                                           std::vector<int> hierarchy, std::vector<int> risk, int hid, 
                                           int th, double swaprate, int seed = 123456)
```

* The parameter `data` contains the household data and is used internal such that `data[0]` contains values for each individual for the first column of the data,
`data[1]` contains the values for each individual for the second column of the data, and so on.
* The procedure expects the data to be **sorted** by household ID (column `hid`). Sorting inside each household is irrelevant. 
* Various internal help functions are included. These are however not intended for flexible use and are only called from withing `recordSwap()`                                           


