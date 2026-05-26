#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_convert_pwlobj(SEXP x) {
  // Initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);

  // Preliminary processing
  std::size_t n_pwlobj_terms = ptr->_pwlobj.size();
  std::size_t A_original_ncol = ptr->ncol();
  std::size_t A_original_nrow = ptr->nrow();
  std::size_t A_current_ncol = A_original_ncol;
  std::size_t A_current_nrow = A_original_nrow;
  Rcpp::List pwc_list;
  std::size_t pwc_n_pieces;
  Rcpp::NumericVector pwc_x;
  Rcpp::NumericVector pwc_y;
  Rcpp::NumericVector pwc_idx;
  std::vector<double> pwc_start;
  std::vector<double> pwc_end;
  std::vector<double> pwc_delta;
  std::vector<double> pwc_zeta;

  // Main processing
  for (std::size_t i = 0; i < n_pwlobj_terms; ++i) {
    // extract piecewise linear terms from problem
    pwc_list = Rcpp::as<Rcpp::List>(ptr->_pwlobj[i]);
    pwc_x = Rcpp::as<Rcpp::NumericVector>(pwc_list["x"]);
    pwc_y = Rcpp::as<Rcpp::NumericVector>(pwc_list["y"]);
    pwc_idx = Rcpp::as<Rcpp::NumericVector>(pwc_list["var"]);
    pwc_n_pieces = pwc_x.size() - 1;

    // extract x start values
    pwc_start.resize(pwc_n_pieces);
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      pwc_start[j] = pwc_x[j];
    }

    // extract x end values
    pwc_end.resize(pwc_n_pieces);
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      pwc_end[j] = pwc_x[j + 1];
    }

    // compute slope coefficients
    pwc_delta.resize(pwc_n_pieces);
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      pwc_delta[j] = (pwc_y[j + 1] - pwc_y[j]) / (pwc_x[j + 1] - pwc_x[j]);
    }

    /// calculate intercept coefficients
    pwc_zeta.resize(pwc_n_pieces);
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      pwc_zeta[j] = pwc_y[j] - pwc_delta[j] * pwc_x[j];
    }

    /// add intercept variables for approximation
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_col_ids.push_back("i_" + std::to_string(i + 1));
    }
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_vtype.push_back("B");
    }
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_lb.push_back(0.0);
    }
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_ub.push_back(1.0);
    }

    /// add slope variables for approximation
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_col_ids.push_back("s_" + std::to_string(i + 1));
    }
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_vtype.push_back("S");
    }
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_lb.push_back(pwc_start[j]);
    }
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_ub.push_back(pwc_end[j]);
    }

    // add objective coefficients
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_obj.push_back(pwc_zeta[j]);
    }
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_obj.push_back(pwc_delta[j]);
    }

    // add constraint (4)
    ptr->_A_i.push_back(A_current_nrow);
    ptr->_A_j.push_back(pwc_idx[0] - 1.0);
    ptr->_A_x.push_back(-1.0);
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_A_i.push_back(A_current_nrow);
      ptr->_A_j.push_back(A_current_ncol + pwc_n_pieces + j);
      ptr->_A_x.push_back(1.0);
    }
    ptr->_sense.push_back("=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("p1_" + std::to_string(i + 1));
    ++A_current_nrow;

    // add constraint (5a)
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      /// coefficient for intercept term
      ptr->_A_i.push_back(A_current_nrow);
      ptr->_A_j.push_back(A_current_ncol + j);
      ptr->_A_x.push_back(-1.0 * pwc_start[j]);
      /// coefficient for slope term
      ptr->_A_i.push_back(A_current_nrow);
      ptr->_A_j.push_back(A_current_ncol + pwc_n_pieces + j);
      ptr->_A_x.push_back(1.0);
      /// additional constraint information
      ptr->_sense.push_back(">=");
      ptr->_rhs.push_back(0.0);
      ptr->_row_ids.push_back("p2_" + std::to_string(i + 1));
      ++A_current_nrow;
    }

    // add constraint (5b)
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      /// coefficient for intercept term
      ptr->_A_i.push_back(A_current_nrow);
      ptr->_A_j.push_back(A_current_ncol + j);
      ptr->_A_x.push_back(-1.0 * pwc_end[j]);
      /// coefficient for slope term
      ptr->_A_i.push_back(A_current_nrow);
      ptr->_A_j.push_back(A_current_ncol + pwc_n_pieces + j);
      ptr->_A_x.push_back(1.0);
      /// additional constraint information
      ptr->_sense.push_back(">=");
      ptr->_rhs.push_back(0.0);
      ptr->_row_ids.push_back("p3_" + std::to_string(i + 1));
      ++A_current_nrow;
    }

    // add constraint (6)
    for (std::size_t j = 0; j < pwc_n_pieces; ++j) {
      ptr->_A_i.push_back(A_current_nrow);
      ptr->_A_j.push_back(A_current_ncol + j);
      ptr->_A_x.push_back(1.0);
    }
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(1.0);
    ptr->_row_ids.push_back("p4_" + std::to_string(i + 1));
    ++A_current_nrow;

    // increment number of columns for next pwl component
    A_current_ncol += pwc_n_pieces + pwc_n_pieces;
  }

  /// remove pwlobj terms
  ptr->_pwlobj = Rcpp::List::create();

  // return success
  return true;
}
