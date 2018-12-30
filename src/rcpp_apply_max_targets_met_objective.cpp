#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_max_targets_met_objective(SEXP x,
                                          Rcpp::List targets_list,
                                          Rcpp::NumericVector costs,
                                          double budget,
                                          Rcpp::NumericVector feature_weights) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];


  // calculate number of non-tip branches
  std::size_t n_nontip_branches = ptr->_number_of_branches -
                                  ptr->_number_of_features;

  // add objective function
  for (std::size_t i = 0;
       i < (ptr->_number_of_actions) +
           (ptr->_number_of_projects) +
           ((ptr->_number_of_projects) * (ptr->_number_of_features)); ++i) {
    ptr->_obj.push_back(0.0);
  }
  for (std::size_t i = 0; i < (ptr->_number_of_features); ++i)
    ptr->_obj.push_back(feature_weights[i]);

  // add constraints for feature variables
  std::size_t r = std::find(ptr->_row_ids.begin(), ptr->_row_ids.end(), "c4") -
      (ptr->_row_ids.begin());
  --r;
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f) {
    r += 1;
    ptr->_A_i.push_back(r);
    ptr->_A_j.push_back((ptr->_number_of_actions) +
                       (ptr->_number_of_projects) +
                       ((ptr->_number_of_features) *
                        (ptr->_number_of_projects)) +
                       f);
    ptr->_A_x.push_back(-1.0 * targets_value[f]);
    ptr->_sense.push_back(Rcpp::as<std::string>(targets_sense[f]));
    ptr->_rhs.push_back(0.0);
  }

  // add column labels for new feature variables
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f)
    ptr->_col_ids.push_back("f");

  // add upper and lower bonds for new feature variables
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f)
    ptr->_lb.push_back(0.0);
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f)
    ptr->_ub.push_back(1.0);

  // add variable types for new feature variables
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f)
    ptr->_vtype.push_back("B");

  // check that we do not have any variables non-tip branches
  if (n_nontip_branches > 0) {
    Rcpp::stop("only tip branches should exist for max targets met objective");
  }

  // add constraints for cost variable
  r = ptr->nrow();
  for (std::size_t i = 0; i < ptr->_number_of_actions; ++i) {
    ptr->_A_i.push_back(r);
    ptr->_A_j.push_back(i);
    ptr->_A_x.push_back(costs[i]);
  }
  ptr->_sense.push_back("<=");
  ptr->_rhs.push_back(budget);
  ptr->_row_ids.push_back("m");

  // assign model sense
  ptr->_modelsense = "max";

  // return succes
  return true;
}
