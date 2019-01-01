#ifndef PACKAGE_H
#define PACKAGE_H

/* Load header files, set plugins, load Rcpp namespace */
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

#endif
