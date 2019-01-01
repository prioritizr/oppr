#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::LogicalMatrix rcpp_random_min_set_solution(
  Rcpp::NumericVector costs,
  arma::sp_mat pa_matrix,
  arma::sp_mat pf_matrix,
  Rcpp::List targets_list,
  Rcpp::IntegerVector locked_in,
  Rcpp::IntegerVector locked_out,
  std::size_t n_solutions,
  bool verbose) {
  // Initialization
  //// extract variables
  std::size_t n_actions = costs.size();
  Rcpp::NumericVector targets_value = targets_list["value"];
  //// declare variables used in the main loop
  std::size_t curr_action;
  std::size_t n_remaining_actions;
  bool curr_targets_unmet;
  arma::mat curr_feature_shortfalls;
  arma::sp_mat::iterator curr_itr;
  arma::sp_mat curr_remaining_actions;
  arma::sp_mat curr_solution;
  //// initialize progress bar
  Progress pb(n_solutions * static_cast<std::size_t>(verbose),
                        verbose);

  // Preliminary processing
  /// initialize solutions matrix
  Rcpp::LogicalMatrix out(n_solutions, n_actions);
  std::fill(out.begin(), out.end(), FALSE);

  /// add locked in actions
  for (auto itr = locked_in.begin(); itr != locked_in.end(); ++itr)
    for (std::size_t j = 0; j < n_solutions; ++j)
      out(j, (*itr) - 1) = TRUE;

  /// add zero cost actions
  for (std::size_t i = 0; i < n_actions; ++i)
    if (costs[i] < 1.0e-15)
      for (std::size_t j = 0; j < n_solutions; ++j)
        out(j, i) = TRUE;

  /// starting actions matrix
  /// initialize matrix
  arma::sp_mat initial_starting_actions(1, n_actions);
  for (std::size_t i = 0; i < n_actions; ++i)
    initial_starting_actions(0, i) = 0.0;

  /// add locked in actions
  for (auto itr = locked_in.begin(); itr != locked_in.end(); ++itr)
    initial_starting_actions(0, (*itr) - 1) = 1.0;

  /// add actions with zero cost
  for (std::size_t i = 0; i < n_actions; ++i)
    if (costs[i] < 1.0e-15)
      initial_starting_actions(0, i) = 1.0;

  /// remaining actions matrix
  //// initialize matrix
  arma::sp_mat initial_remaining_actions(1, n_actions);
  for (std::size_t i = 0; i < n_actions; ++i)
    initial_remaining_actions(0, i) = 1.0;

  //// remove locked in actions
  for (auto itr = locked_in.begin(); itr != locked_in.end(); ++itr)
    initial_remaining_actions.col((*itr) - 1).zeros();

  //// remove actions with zero cost
  for (std::size_t i = 0; i < n_actions; ++i)
    if (costs[i] < 1.0e-15)
      initial_remaining_actions.col(i).zeros();

  /// /remove locked out actions
  for (auto itr = locked_out.begin(); itr != locked_out.end(); ++itr)
    initial_remaining_actions.col((*itr) - 1).zeros();

  // Main processing
  for (std::size_t y = 0; y < n_solutions; ++y) {
    /// check for user interrupt
    if (y % 1000 == 0)
      if (Progress::check_abort())
        Rcpp::stop("user interupt");

    /// initialize variables for generating new random solution
    curr_solution = initial_starting_actions;
    curr_remaining_actions = initial_remaining_actions;
    n_remaining_actions = curr_remaining_actions.n_nonzero;
    curr_feature_shortfalls = expected_persistences_shortfalls(
      pa_matrix, pf_matrix, targets_value, curr_solution);
    curr_targets_unmet = curr_feature_shortfalls.max() > 0;

    //// generate random solutions
    while (curr_targets_unmet & (n_remaining_actions > 0)) {
      //// randomly select solution
      curr_action = std::floor(
        R::runif(0.0, static_cast<double>(n_remaining_actions) - 1.0e-15));
      curr_itr = curr_remaining_actions.begin();
      for (std::size_t counter = 0; counter != curr_action; ++counter)
        ++curr_itr;
      curr_action = curr_itr.col();

      //// remove the selected action from the remaining actions
      curr_remaining_actions.col(curr_action).zeros();
      --n_remaining_actions;

      //// add the action to the current solution
      curr_solution(0, curr_action) = 1.0;

      //// add action to solutions matrix
      out(y, curr_action) = TRUE;

      //// determine if the targets are still unmet with the newly added action
      curr_feature_shortfalls = expected_persistences_shortfalls(
        pa_matrix, pf_matrix, targets_value, curr_solution);
      curr_targets_unmet = curr_feature_shortfalls.max() > 0;
    }

    //// increment progress bar
    pb.increment();
  }

  // Exports
  return out;
}
