#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_locked_action_constraints(SEXP x,
                                          Rcpp::IntegerVector actions,
                                          Rcpp::NumericVector status) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t n = actions.size();

  // apply constraints
  for (std::size_t i = 0; i < n; ++i) {
    if (status[i] < 0.5) {
      ptr->_ub[actions[i] - 1] = 0.0;
    } else {
      ptr->_lb[actions[i] - 1] = 1.0;
    }
  }

  // return success
  return true;
}
