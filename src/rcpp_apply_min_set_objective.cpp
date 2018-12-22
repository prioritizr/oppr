#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_min_set_objective(SEXP x, Rcpp::List targets_list,
                                  Rcpp::NumericVector costs) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_nrow = ptr->_rhs.size();
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];

  // add objective function
  for (std::size_t i = 0; i < (ptr->_number_of_actions); ++i)
    ptr->_obj.push_back(costs[i]);

  // add target constraints
  std::size_t r = A_original_nrow - 1;
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f) {
    ++r;
    ptr->_A_i.push_back(r);
    ptr->_A_j.push_back(r);
    ptr->_A_x.push_back(1.0);
    ptr->_sense.push_back(Rcpp::as<std::string>(targets_sense[f]));
    ptr->_rhs.push_back(targets_value[f]);
    ptr->_row_ids.push_back("target");
  }

  // assign model sense
  ptr->_modelsense="min";

  // return succes
  return true;
}
