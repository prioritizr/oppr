#pragma once
#ifndef PACKAGE_H
#define PACKAGE_H

/* Load header files, set plugins, load Rcpp namespace */
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <cmath>

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

// small number to determine if a double is greater than zero
#define SMALL_TOL 1.0e-6

#endif
