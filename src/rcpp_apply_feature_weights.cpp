#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_feature_weights(SEXP x, Rcpp::NumericVector weights,
                                bool replace) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);

  // apply weights
  if (replace) {
    for (std::size_t f = 0; f < (ptr->_number_of_features); ++f) {
      ptr->_obj[(ptr->_number_of_actions) +
                (ptr->_number_of_projects) +
                (ptr->_number_of_projects * ptr->_number_of_features) +
                f] = weights[f];
    }
  } else {
    for (std::size_t f = 0; f < (ptr->_number_of_features); ++f) {
      ptr->_obj[(ptr->_number_of_actions) +
                (ptr->_number_of_projects) +
                (ptr->_number_of_projects * ptr->_number_of_features) +
                f] += weights[f];
    }
  }

  // return success
  return true;
}
