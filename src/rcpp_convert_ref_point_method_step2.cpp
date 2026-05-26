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

  // Add a linear constraint based on the current objective
  // which currentlyu corresponds to the min-max component of the
  // reference point method
  for (std::size_t i = 0; i < A_ncol; ++i) {
    if (std::abs(ptr->_obj[i]) >= SMALL_TOL) {
      ptr->_A_i.push_back(A_nrow);
      ptr->_A_j.push_back(i);
      ptr->_A_x.push_back(ptr->_obj[i]);
    }
  }
  ptr->_rhs.push_back(rhs);
  ptr->_sense.push_back("<=");
  ptr->_row_ids.push_back("max");

  // Reset objective
  for (std::size_t i = 0; i < A_ncol; ++i) {
    ptr->_obj[i] = 0.0;
  }

  // Set objective to be to minimize the weighted sum of the goal
  // shortfall variables
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_obj[shortfall_idx + i] = weights[i];
  }

  // return success
  return true;
}
