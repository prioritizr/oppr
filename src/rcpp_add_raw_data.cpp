#include "package.h"
#include "functions.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_add_raw_data(SEXP x, arma::sp_mat pa_matrix, arma::sp_mat pf_matrix) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // assign indices
  ptr->_number_of_projects = static_cast<std::size_t>(pa_matrix.n_rows);
  ptr->_number_of_actions = static_cast<std::size_t>(pa_matrix.n_cols);
  ptr->_number_of_features = static_cast<std::size_t>(pf_matrix.n_cols);

  // set up problem with raw data
  std::size_t r = -1;

  //// add column ids
  for (std::size_t i = 0; i < (ptr->_number_of_actions); ++i)
    ptr->_col_ids.push_back("i");
  for (std::size_t i = 0; i < (ptr->_number_of_projects); ++i)
    ptr->_col_ids.push_back("j");
  for (std::size_t i = 0;
       i < ((ptr->_number_of_features) *(ptr->_number_of_projects)); ++i)
    ptr->_col_ids.push_back("fj");
  for (std::size_t i = 0; i < (ptr->_number_of_features); ++i)
    ptr->_col_ids.push_back("f");

  //// constraints to ensure that projects can only be funded if all of their
  //// actions are funded
  for (auto pitr = pa_matrix.begin(); pitr != pa_matrix.end(); ++pitr) {
    if ((*pitr) > 0.5) {
      r += 1;
      ptr->_A_i.push_back(r);
      ptr->_A_i.push_back(r);
      ptr->_A_j.push_back(pitr.col());
      ptr->_A_j.push_back((ptr->_number_of_actions) + pitr.row());
      ptr->_A_x.push_back(1.0);
      ptr->_A_x.push_back(-1.0);
      ptr->_sense.push_back(">=");
      ptr->_rhs.push_back(0.0);
      ptr->_row_ids.push_back("c1");
    }
  }

  //// constraints to ensure that features can only be allocated to funded
  //// projects
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f) {
    for (std::size_t p = 0; p < (ptr->_number_of_projects); ++p) {
      r += 1;
      ptr->_A_i.push_back(r);
      ptr->_A_i.push_back(r);
      ptr->_A_j.push_back((ptr->_number_of_actions) + p);
      ptr->_A_j.push_back((ptr->_number_of_actions) +
                          (ptr->_number_of_projects) +
                          (f * (ptr->_number_of_projects)) + p);
      ptr->_A_x.push_back(1.0);
      ptr->_A_x.push_back(-1.0);
      ptr->_sense.push_back(">=");
      ptr->_rhs.push_back(0.0);
      ptr->_row_ids.push_back("c2");
    }
  }

  //// constraints to ensure that each feature can only be allocated to a single
  //// project
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f) {
    r += 1;
    for (std::size_t p = 0; p < (ptr->_number_of_projects); ++p) {
      ptr->_A_i.push_back(r);
      ptr->_A_j.push_back((ptr->_number_of_actions) +
                          (ptr->_number_of_projects) +
                          (f * (ptr->_number_of_projects)) + p);
      ptr->_A_x.push_back(1.0);
    }
    ptr->_sense.push_back("=");
    ptr->_row_ids.push_back("c3");
    ptr->_rhs.push_back(1.0);
  }

  /// constraints for persistence probabilities for features
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f) {
    //// increment row
    r += 1;
    //// apply constraint for the feature
    for (auto pitr = pf_matrix.begin_col(f);
         pitr != pf_matrix.end_col(f); ++pitr) {
        ptr->_A_i.push_back(r);
        ptr->_A_j.push_back((ptr->_number_of_actions) +
                            (ptr->_number_of_projects) +
                            (f * (ptr->_number_of_projects)) +
                            pitr.row());
        ptr->_A_x.push_back(*pitr);
    }
    ptr->_A_i.push_back(r);
    ptr->_A_j.push_back(
      (ptr->_number_of_actions) +
      (ptr->_number_of_projects) +
      ((ptr->_number_of_features) * (ptr->_number_of_projects)) +
      f);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("c4");
  }

  // return result
  return true;
}
