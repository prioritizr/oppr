#include "package.h"
#include "functions.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_add_raw_data(SEXP x, arma::sp_mat pa_matrix, arma::sp_mat pf_matrix,
                       arma::sp_mat branch_matrix,
                       Rcpp::NumericVector branch_lengths,
                       std::size_t n_approx_points) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);

  // assign indices
  ptr->_number_of_projects = static_cast<std::size_t>(pa_matrix.n_rows);
  ptr->_number_of_actions = static_cast<std::size_t>(pa_matrix.n_cols);
  ptr->_number_of_features = static_cast<std::size_t>(pf_matrix.n_cols);
  ptr->_number_of_branches = branch_lengths.size();

  /// identify branches that are not tips
  std::size_t n_branch_nontips = (ptr->_number_of_branches) -
                                 (ptr->_number_of_features);
  std::vector<std::size_t> branch_nontip_indices(n_branch_nontips);
  std::iota(branch_nontip_indices.begin(), branch_nontip_indices.end(),
            (ptr->_number_of_features));

  // set up problem with raw data
  int r = -1;

  //// add column ids
  for (std::size_t i = 0; i < (ptr->_number_of_actions); ++i)
    ptr->_col_ids.push_back("i");
  for (std::size_t i = 0; i < (ptr->_number_of_projects); ++i)
    ptr->_col_ids.push_back("j");
  for (std::size_t i = 0;
       i < ((ptr->_number_of_features) *(ptr->_number_of_projects)); ++i)
    ptr->_col_ids.push_back("fj");

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
      if (pf_matrix(p, f) > 1.0e-15) {
        ptr->_A_i.push_back(r);
        ptr->_A_j.push_back((ptr->_number_of_actions) +
                            (ptr->_number_of_projects) +
                            (f * (ptr->_number_of_projects)) + p);
        ptr->_A_x.push_back(1.0);
      }
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
    ptr->_row_ids.push_back("c4");
  }

  /// constraints for the log-sum probabilities for nontip branches
  std::vector<std::size_t> model_pwl_var;
  model_pwl_var.reserve(n_branch_nontips);
  std::vector<std::vector<double>> model_pwl_x(n_branch_nontips);
  std::vector<std::vector<double>> model_pwl_y(n_branch_nontips);
  double curr_min_value;
  double curr_max_value;
  double curr_frac;
  double curr_tmp_value;
  int p = -1;
  if (n_branch_nontips > 0) {
    /// initialize variables
    for (auto bitr = branch_nontip_indices.begin(); bitr !=
         branch_nontip_indices.end(); ++bitr) {
      //// increment counters
      ++p;
      /// calculate the log(1 - min) and log(1 - max) extinction probabilties
      /// probabilities for each phylogenetic branch,
      // note that we substract 1e-15 if the probabilities are exactly 1
      curr_min_value = 0.0;
      curr_max_value = 0.0;
      for (auto sitr = branch_matrix.begin_col(*bitr);
           sitr != branch_matrix.end_col(*bitr);
           ++sitr) {
        /// calculate highest possible persistence probabilitity
        curr_tmp_value = pf_matrix.col(sitr.row()).max();
        if (std::abs(1.0 - curr_tmp_value) < 1.0e-15)
          curr_tmp_value = 1.0 - 1.0e-15;
        curr_min_value += std::log(1.0 - curr_tmp_value);
        /// calculate lowest possible persistence probability
        curr_tmp_value = 100.0;
        for (auto pitr = pf_matrix.begin_col(sitr.row());
             pitr != pf_matrix.end_col(sitr.row()); ++pitr) {
          if (*pitr < curr_tmp_value) {
            curr_tmp_value = *pitr;
          }
        }
        if (std::abs(1.0 - curr_tmp_value) < 1.0e-15)
          curr_tmp_value = 1.0 - 1.0e-15;
        curr_max_value += std::log(1.0 - curr_tmp_value);
      }

      /// if min and max values differ by less than 1e-6 then we need
      /// to store some values that will later be modified by the
      /// phylogenetic objective c++ function, this is extraordinarily hacky
      /// but Gurobi doesn't seem to return valid solutions when
      /// a variable is constant and is associated with a piece-wise linear
      /// component in the objective function
      if (std::abs(curr_min_value - curr_max_value) < 1.0e-6) {
        /// store variable
        model_pwl_var.push_back((ptr->_number_of_actions) +
                                (ptr->_number_of_projects) +
                                (ptr->_number_of_features *
                                (ptr->_number_of_projects)) +
                                (*bitr) + 1.0);

        /// pre-allocate vectors for xy pwl data
        model_pwl_x[p].reserve(1);
        model_pwl_y[p].reserve(1);

        /// store constant value in y
        model_pwl_y[p].push_back(branch_lengths[*bitr] *
                                 (1.0 - std::exp(curr_min_value)));

        /// store a missing value in x to flag that this branch needs special
        /// handling.
        model_pwl_x[p].push_back(std::numeric_limits<double>::quiet_NaN());

        /// please see rcpp_apply_max_phylo_div_objective.cpp to see how
        /// the quiet NaN is handled

      } else {
        /// otherwise, add constraints and piece-wise linear objective function

        //// increment variable
        ++r;

        //// apply linear constraints to ensure correct calculatiosn of the
        //// piece-wise linear objective function
        for (auto sitr = branch_matrix.begin_col(*bitr);
             sitr != branch_matrix.end_col(*bitr); ++sitr) {
          for (auto pitr = pf_matrix.begin_col(sitr.row());
               pitr != pf_matrix.end_col(sitr.row()); ++pitr) {
            ptr->_A_i.push_back(r);
            ptr->_A_j.push_back((ptr->_number_of_actions) +
                                (ptr->_number_of_projects) +
                                (sitr.row() * (ptr->_number_of_projects)) +
                                pitr.row());
            //// if probability of persistence is really close to 1, so that
            //// log(1 - prob) is Inf, then replace with number that is
            //// very close but not equal to 1
            curr_tmp_value = (*pitr);
            if (std::abs(1.0 - curr_tmp_value) < 1.0e-15)
              curr_tmp_value = 1.0 - 1.0e-15;
            // add log value
            ptr->_A_x.push_back(std::log(1.0 - curr_tmp_value));
          }
        }
        ptr->_row_ids.push_back("c5");

        // first, slightly expand the range, note that these values
        // should be negative since they range betwen log(1) and log(1e-15)
        curr_min_value *= 1.01;
        curr_max_value *= 0.99;

        /// pre-allocate vectors for xy pwl data
        model_pwl_x[p].reserve(n_approx_points);
        model_pwl_y[p].reserve(n_approx_points);

        /// add pwl objective function
        model_pwl_var.push_back((ptr->_number_of_actions) +
                                (ptr->_number_of_projects) +
                                (ptr->_number_of_features *
                                (ptr->_number_of_projects)) +
                                (*bitr) + 1.0);
        curr_frac = (curr_max_value - curr_min_value) /
                    static_cast<double>(n_approx_points - 1);
        for (std::size_t i = 0; i < n_approx_points; ++i)
          model_pwl_x[p].push_back(curr_min_value +
                                   (static_cast<double>(i) * curr_frac));
        for (std::size_t i = 0; i < n_approx_points; ++i)
          model_pwl_y[p].push_back(branch_lengths[*bitr] *
                                   (1.0 - std::exp(model_pwl_x[p][i])));
      }
    }
  }

  // store pwl data
  ptr->_pwlobj = Rcpp::List(n_branch_nontips);
  for (std::size_t i = 0; i < n_branch_nontips; ++i)
    ptr->_pwlobj[i] = Rcpp::List::create(
      Rcpp::Named("var") = model_pwl_var[i],
      Rcpp::Named("x") = model_pwl_x[i],
      Rcpp::Named("y") = model_pwl_y[i]);

  // return result
  return true;
}
