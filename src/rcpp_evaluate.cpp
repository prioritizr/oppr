#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_evaluate_max_phylo_div_objective(
  Rcpp::NumericVector costs, arma::sp_mat pa_matrix, arma::sp_mat pf_matrix,
  arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths,
  Rcpp::NumericVector targets, Rcpp::NumericVector weights,
  arma::sp_mat solutions) {
  return evaluate_max_phylo_div_objective(
    costs, pa_matrix, pf_matrix, branch_matrix, branch_lengths, targets,
    weights, solutions);
}

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_evaluate_min_set_objective(
  Rcpp::NumericVector costs, arma::sp_mat pa_matrix, arma::sp_mat pf_matrix,
  arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths,
  Rcpp::NumericVector targets, Rcpp::NumericVector weights,
  arma::sp_mat solutions) {
  return evaluate_min_set_objective(
    costs, pa_matrix, pf_matrix, branch_matrix, branch_lengths, targets,
    weights, solutions);
}

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_evaluate_max_targets_met_objective(
  Rcpp::NumericVector costs, arma::sp_mat pa_matrix, arma::sp_mat pf_matrix,
  arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths,
  Rcpp::NumericVector targets, Rcpp::NumericVector weights,
  arma::sp_mat solutions) {
  return evaluate_max_targets_met_objective(
    costs, pa_matrix, pf_matrix, branch_matrix, branch_lengths, targets,
    weights, solutions);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_expected_persistences(
  arma::sp_mat pa_matrix, arma::sp_mat pf_matrix, arma::sp_mat branch_matrix,
  arma::sp_mat solutions) {
  return Rcpp::wrap(expected_persistences(
    pa_matrix, pf_matrix, branch_matrix, solutions));
}
