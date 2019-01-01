#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include "package.h"
#include <vector>
#include <math.h>
#include <string>
#include <algorithm>

// power function, templated to use compile-time loop unrolling
template<int P>
inline double Pow(double x) {
  return (Pow<P-1>(x) * x);
}

template<>
inline double Pow<1>(double x) {
	return (x);
}

template<>
inline double Pow<0>(double x) {
  return (1.0);
}

// calculate euclidean distance
double distance(double, double, double, double);

// convert object to string
template<typename T>
inline std::string num2str(T number, int precision=10) {
  std::ostringstream ss;
  ss << std::fixed << std::setprecision(precision) << number;
  return(ss.str());
}

Rcpp::NumericVector evaluate_max_phylo_div_objective(
  Rcpp::NumericVector, arma::sp_mat, arma::sp_mat, arma::sp_mat,
  Rcpp::NumericVector, Rcpp::NumericVector, Rcpp::NumericVector,
  arma::sp_mat);

Rcpp::NumericVector evaluate_max_targets_met_objective(
  Rcpp::NumericVector, arma::sp_mat, arma::sp_mat, arma::sp_mat,
  Rcpp::NumericVector, Rcpp::NumericVector, Rcpp::NumericVector,
  arma::sp_mat);

Rcpp::NumericVector evaluate_min_set_objective(
  Rcpp::NumericVector, arma::sp_mat, arma::sp_mat, arma::sp_mat,
  Rcpp::NumericVector, Rcpp::NumericVector, Rcpp::NumericVector,
  arma::sp_mat);

// calculate shortfall in expected persistences
arma::mat expected_persistences_shortfalls(
  arma::sp_mat, arma::sp_mat, Rcpp::NumericVector, arma::sp_mat);

arma::mat expected_persistences(
  arma::sp_mat, arma::sp_mat, arma::sp_mat, arma::sp_mat);

#endif
