#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_max_phylo_div_objective(SEXP x,
                                        Rcpp::NumericVector costs,
                                        double budget,
                                        Rcpp::NumericVector feature_weights) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
   Rcpp::List curr_pwl_list;
   Rcpp::NumericVector curr_pwl_x;
   Rcpp::NumericVector curr_pwl_y;

  // calculate number of non-tip branches
  std::size_t n_nontip_branches = ptr->_number_of_branches -
                                  ptr->_number_of_features;

  // create a logical vector to determine which pwlobj's should be kept,
  // but also avoid creating one with a size of zeros, in case this causes
  // problems
  Rcpp::LogicalVector keep_pwl(std::max(n_nontip_branches,
                                        static_cast<std::size_t>(1)));

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
    ++r;
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

  /// find row to start adding constraints
  r = std::find(ptr->_row_ids.begin(), ptr->_row_ids.end(),
                "c5") - (ptr->_row_ids.begin());
  --r;

  // add variables and constraints for non-tip branches if they exist
  if (n_nontip_branches > 0) {

    /// iterate over each phylogenetic branch
    for (std::size_t b = 0; b < n_nontip_branches; ++b) {

      /// extract piece-wise linear data from the objective function
      curr_pwl_list = Rcpp::as<Rcpp::List>(ptr->_pwlobj[b]);
      curr_pwl_x = Rcpp::as<Rcpp::NumericVector>(curr_pwl_list["x"]);
      curr_pwl_y = Rcpp::as<Rcpp::NumericVector>(curr_pwl_list["y"]);

      /// if the branch has a non-constant probability of persistence,
      /// then use the following code to encode the piece-wise linear objective
      /// function
      if (!NumericVector::is_na(curr_pwl_x[0])) {
        /// keep the pwlobj
        keep_pwl[b] = 1;

        /// add additional zeros to the linear component of the objective
        /// function for the new branch variables
        ptr->_obj.push_back(0.0);

        /// add bounds for the new branch variables
        ptr->_lb.push_back(-std::numeric_limits<double>::infinity());
        ptr->_ub.push_back(std::numeric_limits<double>::infinity());

        /// add types for the new branch variables
        ptr->_vtype.push_back("C");

        /// add names for the new branch variables
        ptr->_col_ids.push_back("b");

        /// add extra constraints for the piece-wise linear function
        ++r;
        ptr->_A_i.push_back(r);
        ptr->_A_j.push_back((ptr->_number_of_actions) +
                            (ptr->_number_of_projects) +
                            (ptr->_number_of_features *
                             ptr->_number_of_projects) +
                            (ptr->_number_of_features) + b);
        ptr->_A_x.push_back(-1.0);
        ptr->_sense.push_back("=");
        ptr->_rhs.push_back(0.0);
      } else {

        /// do not keep the pwlobj
        keep_pwl[b] = 0;

        /// for the new branch variables
        ptr->_obj.push_back(curr_pwl_y[0]);

        /// add bounds for the new branch variables
        ptr->_lb.push_back(1.0);
        ptr->_ub.push_back(1.0);

        /// add types for the new branch variables
        ptr->_vtype.push_back("B");

        /// add names for the new branch variables
        ptr->_col_ids.push_back("b");

      }
    }

    /// remove pwlobjs if needed
    ptr->_pwlobj = ptr->_pwlobj[keep_pwl];
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
