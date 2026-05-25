#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_convert_ref_point_method_step1(
  SEXP x,
  Rcpp::CharacterVector mopt_modelsense,
  Rcpp::NumericMatrix mopt_obj,
  Rcpp::NumericVector weights,
  Rcpp::NumericVector goals,
  Rcpp::NumericVector best,
  Rcpp::NumericVector worst
) {
  // Note that this function assumes that all modelsense values
  // are equal to "max" and so it only works for when all objectives
  // are for maximization problems

  // Initialization
  /// define counters
  const std::size_t n = mopt_modelsense.size();
  /// import optimization problem
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  /// store original problem size
  std::size_t A_ncol = ptr->ncol();
  std::size_t A_nrow = ptr->nrow();

  // Set model sense
  ptr->_modelsense = "min";

  // Define additional decision variables for shortfall variables
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_lb.push_back(0.0);
    // note that we inflate shortfall to account for precision loss due to pwl
    ptr->_ub.push_back(std::max(best[i] - worst[i], 0.0) * 1.5);
    ptr->_vtype.push_back("C");
    ptr->_col_ids.push_back("sh");
  }

  // Define additional decision variables for maximum value
  // compute upper bound
  double ub = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    ub = std::max(
      ub,
      // note that we inflate shortfall to account for precision loss due to pwl
      weights[i] * std::max(best[i] - worst[i], 0.0) * 1.5
    );
  }

  // compute apply constraint
  ptr->_ub.push_back(ub);
  ptr->_lb.push_back(0.0);
  ptr->_vtype.push_back("C");
  ptr->_col_ids.push_back("mobj");

  // Specify objective coefficients
  for (std::size_t i = 0; i < A_ncol; ++i) {
    ptr->_obj[i] = 0.0;
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_obj.push_back(0.0);
  }
  ptr->_obj.push_back(1.0);

  // Add linear constraints for calculating shortfall of goals
  for (std::size_t j = 0; j < A_ncol; ++j) {
    for (std::size_t i = 0; i < n; ++i) {
      if (std::abs(mopt_obj(i, j)) >= SMALL_TOL) {
        ptr->_A_i.push_back(A_nrow + i);
        ptr->_A_j.push_back(j);
        ptr->_A_x.push_back(mopt_obj(i, j));
      }
    }
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_A_i.push_back(A_nrow + i);
    ptr->_A_j.push_back(A_ncol + i);
    ptr->_A_x.push_back(1.0);
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_rhs.push_back(goals[i]);
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_sense.push_back(">=");
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_row_ids.push_back("sh");
  }

  // Add linear constraints for calculating the maximum of weighted shortfalls
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_A_i.push_back(A_nrow + n + i);
    ptr->_A_j.push_back(A_ncol + n);
    ptr->_A_x.push_back(1.0);
    ptr->_A_i.push_back(A_nrow + n + i);
    ptr->_A_j.push_back(A_ncol + i);
    ptr->_A_x.push_back(-1.0 * weights[i]);
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_rhs.push_back(0.0);
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_sense.push_back(">=");
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_row_ids.push_back("max");
  }

  // return success
  return true;
}
