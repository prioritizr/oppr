#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_convert_ref_point_method_step2(
  SEXP x,
  Rcpp::CharacterVector mopt_modelsense,
  Rcpp::NumericMatrix mopt_obj,
  Rcpp::NumericVector weights,
  Rcpp::NumericVector goals,
  double rhs
) {
  // Initialization
  /// define counters
  const std::size_t n = mopt_modelsense.size();
  /// import optimization problem
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  /// store original problem size
  std::size_t A_ncol = ptr->ncol();
  std::size_t A_nrow = ptr->nrow();
  /// compute shortfall variable starting index
  std::size_t shortfall_idx = A_ncol - 1 - n;

  // Remove linear constraints for step 1
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_A_i.pop_back();
    ptr->_A_i.pop_back();
    ptr->_A_j.pop_back();
    ptr->_A_j.pop_back();
    ptr->_A_x.pop_back();
    ptr->_A_x.pop_back();
    ptr->_rhs.pop_back();
    ptr->_sense.pop_back();
    ptr->_row_ids.pop_back();
  }

  // Add a linear constraint for each shortfall variable
  A_nrow = ptr->nrow();
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_A_i.push_back(A_nrow + i);
    ptr->_A_j.push_back(shortfall_idx + i);
    ptr->_A_x.push_back(1.0);
    ptr->_rhs.push_back(rhs);
    ptr->_sense.push_back("<=");
    ptr->_row_ids.push_back("max");
  }

  // Reset objective
  for (std::size_t i = 0; i < A_ncol; ++i) {
    ptr->_obj[i] = 0.0;
  }

  // Set objective to be to minimize the weighted sum of the goal
  // shortfall variables
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_obj[shortfall_idx + i] = weights[i];
  }

  // Update upper bounds for shortfall variables
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_ub[shortfall_idx + i] = rhs;
  }

  // return success
  return true;
}
