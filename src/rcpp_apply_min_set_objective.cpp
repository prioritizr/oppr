#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_min_set_objective(SEXP x, Rcpp::List targets_list,
                                  Rcpp::NumericVector costs) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];

  // add objective function
  for (std::size_t i = 0; i < (ptr->_number_of_actions); ++i)
    ptr->_obj.push_back(costs[i]);
  for (std::size_t i = 0;
       i < (ptr->_number_of_projects) +
           ((ptr->_number_of_projects) * (ptr->_number_of_features)); ++i) {
    ptr->_obj.push_back(0.0);
  }

  // add targets for features
  std::size_t r = std::find(ptr->_row_ids.begin(), ptr->_row_ids.end(), "c4") -
      (ptr->_row_ids.begin());
  --r;
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f) {
    r += 1;
    ptr->_sense.push_back(Rcpp::as<std::string>(targets_sense[f]));
    ptr->_rhs.push_back(targets_value[f]);
  }

  // assign model sense
  ptr->_modelsense="min";

  // return succes
  return true;
}
