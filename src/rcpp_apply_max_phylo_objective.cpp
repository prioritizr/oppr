#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_max_phylo_objective(SEXP x,
                                    Rcpp::NumericVector costs,
                                    double budget,
                                    double default_feature_weight) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);

  // add objective function
  for (std::size_t i = 0;
       i < (ptr->_number_of_actions) +
           (ptr->_number_of_projects) +
           ((ptr->_number_of_projects) * (ptr->_number_of_features)); ++i) {
    ptr->_obj.push_back(0.0);
  }
  for (std::size_t i = 0; i < (ptr->_number_of_features); ++i)
    ptr->_obj.push_back(default_feature_weight);

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
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("=");
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
    ptr->_vtype.push_back("S");

  // add constraints for non-tip branches if they exist
  if (ptr->_number_of_branches > ptr->_number_of_features) {
    /// find row to start adding constraints
    std::size_t r = std::find(ptr->_row_ids.begin(), ptr->_row_ids.end(),
                              "c5") - (ptr->_row_ids.begin()) - 1;

    /// add new constraints for non-tip branches
    for (std::size_t b = 0;
         b < (ptr->_number_of_branches > ptr->_number_of_features); ++b) {
      ptr->_A_i.push_back(r);
      ptr->_A_j.push_back((ptr->_number_of_actions) +
                          (ptr->_number_of_projects) +
                          (ptr->_number_of_features *
                           ptr->_number_of_projects) +
                          (ptr->_number_of_features) + b);
      ptr->_A_x.push_back(0.0);
      ptr->_sense.push_back("=");
      ptr->_rhs.push_back(0.0);
      ptr->_vtype.push_back("C");
    }
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
