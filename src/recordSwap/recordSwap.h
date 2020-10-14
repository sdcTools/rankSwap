/*
 * Open Source Software to apply Statistical Disclosure Control techniques
 * 
 * This program is free software; you can redistribute it and/or 
 * modify it under the terms of the European Union Public Licence 
 * (EUPL) version 1.1, as published by the European Commission.
 * 
 * You can find the text of the EUPL v1.1 on
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 * 
 * This software is distributed on an "AS IS" basis without 
 * warranties or conditions of any kind, either express or implied.
 */

/* 
* Header file for shared library recordSwap.dll
* with source code recordSwap.cpp
* to perform Targeted Record Swapping
*/
#include <iostream>     
#include <algorithm>    // std::count
#include <vector>       // std::vector
#include <random>
#include <queue>
#include <array>
#include <map>
#include <unordered_set>
#include <unordered_map>

#ifndef __RECORDSWAP_H__
#define	__RECORDSWAP_H__

//class CrecordSwap
//{
//public:
/*
* Function to perform record swapping
*/
std::vector<std::vector<int> > recordSwap(std::vector< std::vector<int> > data, std::vector<int> similar,
                                          std::vector<int> hierarchy, std::vector<int> risk, int hid, int th, double swaprate,
                                          std::vector<int> carry_along,
                                          int seed);

//private:
/*
* Function to reorder data-set given one index vector 
*/
std::vector< std::vector<int> > orderData(std::vector< std::vector<int> > &data, int orderIndex);

/*
* Function to define levels 
*/
std::vector<int> setLevels(std::vector< std::vector<double> > &risk, double risk_threshold);

/*
 * Function to set sampling probability 
 * and reverse sampling probability (for donor sets)
 */
std::vector< std::vector<double> > setRisk(std::vector<std::vector<int> > &data, std::vector<int> &hierarchy, std::vector<int> &risk_variables, int &hid);


/*
 * Function to sample from std::vector<int> given a probability vector
 */
std::vector<int> randSample(std::unordered_set<int> &ID, int N, std::vector<double> &prob, std::mt19937 &mersenne_engine,
                            std::vector<int> &IDused, std::unordered_set<int> &mustSwap);

/*
 * Function to sample from donor set
 * this is done differently than the inital sampling to make procedure more efficient
 */
std::vector<int> sampleDonor(std::vector< std::vector<int> > &data, std::vector<int> &similar,
                             std::vector<int> &IDswap, std::unordered_set<int> &IDswap_pool,
                             std::map<double,int> &IDdonor_pool, std::vector<int> &IDused, int &hid);
  
/*
 * Function to distribute n draws over a given number of groups
 * the distribution is always proportional to group size
 */
std::map<std::vector<int>,std::pair<int,int>> distributeDraws(std::map<std::vector<int>,std::unordered_set<int> > &group_hier,
                                                              int &nhid, double &swaprate,
                                                              std::uniform_int_distribution<std::mt19937::result_type> &runif01,
                                                              std::mt19937 &mersenne_engine);
  
#endif /* __RECORDSWAP_H__ */
