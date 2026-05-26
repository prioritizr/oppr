#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_convert_abs_constraint_approach(
  SEXP x,
  Rcpp::CharacterVector mopt_modelsense,
  Rcpp::NumericMatrix mopt_obj,
  Rcpp::NumericVector goals
) {
  // Initialization
  /// define counters
  const std::size_t n = mopt_modelsense.size();
  /// import optimization problem
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  /// store original problem size
  std::size_t A_ncol = ptr->ncol();
  std::size_t A_nrow = ptr->nrow();

  // Set model sense according to primary objective
  ptr->_modelsense = mopt_modelsense[0];

  // Specify objective coefficients according to primary objective
  for (std::size_t i = 0; i < A_ncol; ++i) {
    ptr->_obj[i] = mopt_obj(0, i);
  }

  // Add linear constraints for each of the objectives
  std::size_t curr_row = A_nrow;
  for (std::size_t i = 0; i < n; ++i) {
    if (!NumericVector::is_na(goals[i])) {
      for (std::size_t j = 0; j < A_ncol; ++j) {
        if (mopt_obj(i, j) >= SMALL_TOL) {
          ptr->_A_i.push_back(curr_row);
          ptr->_A_j.push_back(j);
          ptr->_A_x.push_back(mopt_obj(i, j));
        }
      }
      ++curr_row;
    }
  }
  for (std::size_t i = 0; i < n; ++i) {
    if (!NumericVector::is_na(goals[i])) {
      ptr->_rhs.push_back(goals[i]);
    }
  }
  for (std::size_t i = 0; i < n; ++i) {
    if (!NumericVector::is_na(goals[i])) {
      ptr->_sense.push_back(mopt_modelsense[i] == "max" ? ">=" : "<=");
    }
  }
  for (std::size_t i = 0; i < n; ++i) {
    if (!NumericVector::is_na(goals[i])) {
      ptr->_row_ids.push_back("mobj");
    }
  }

  // return success
  return true;
}
