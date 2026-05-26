#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_convert_wtd_goal_method(
  SEXP x,
  Rcpp::CharacterVector mopt_modelsense,
  Rcpp::NumericMatrix mopt_obj,
  Rcpp::NumericVector weights,
  Rcpp::NumericVector goals
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

  // Define additional decision variables
  for (std::size_t i = 0; i < n; ++i) {
    if (mopt_modelsense[i] == "max") {
      ptr->_ub.push_back(1.0);
    } else {
      ptr->_ub.push_back(std::numeric_limits<double>::infinity());
    }
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_lb.push_back(0.0);
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_vtype.push_back("C");
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_col_ids.push_back("mobj");
  }

  // Specify objective coefficients
  for (std::size_t i = 0; i < A_ncol; ++i) {
    ptr->_obj[i] = 0.0;
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_obj.push_back(weights[i]);
  }

  // Add linear constraints
  for (std::size_t j = 0; j < A_ncol; ++j) {
    for (std::size_t i = 0; i < n; ++i) {
      if (std::abs(mopt_obj(i, j)) >= SMALL_TOL) {
        ptr->_A_i.push_back(A_nrow + i);
        ptr->_A_j.push_back(j);
        ptr->_A_x.push_back(mopt_obj(i, j));
      }
    }
  }
  for (std::size_t i =  0; i < n; ++i) {
    ptr->_A_i.push_back(A_nrow + i);
    ptr->_A_j.push_back(A_ncol + i);
    if (mopt_modelsense[i] == "max") {
      ptr->_A_x.push_back(goals[i]);
    } else {
      ptr->_A_x.push_back(-goals[i]);
    }
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_rhs.push_back(goals[i]);
  }
  for (std::size_t i = 0; i < n; ++i) {
    if (mopt_modelsense[i] == "max") {
      ptr->_sense.push_back(">=");
    } else {
      ptr->_sense.push_back("<=");
    }
  }
  for (std::size_t i = 0; i < n; ++i) {
    if (mopt_modelsense[i] == "max") {
      ptr->_row_ids.push_back("sh");
    } else {
      ptr->_row_ids.push_back("ovr");
    }
  }

  // return success
  return true;
}
