#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_locked_project_constraints(SEXP x,
                                           Rcpp::IntegerVector projects,
                                           Rcpp::NumericVector status) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t n = projects.size();
  std::size_t n_actions = ptr->_number_of_actions;

  // apply constraints
  for (std::size_t i = 0; i < n; ++i) {
    if (status[i] < 0.5) {
      ptr->_ub[n_actions + projects[i] - 1] = 0.0;
    } else {
      ptr->_lb[n_actions + projects[i] - 1] = 1.0;
    }
  }

  // return success
  return true;
}
