#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::LogicalMatrix rcpp_random_solution(
  Rcpp::NumericVector costs,
  arma::sp_mat pa_matrix,
  arma::sp_mat pf_matrix,
  Rcpp::NumericVector targets,
  double budget,
  Rcpp::IntegerVector locked_in,
  Rcpp::IntegerVector locked_out,
  std::size_t number_solutions,
  bool verbose,
  const std::string obj_name) {
  // Initialization
  std::size_t n_actions = costs.size();
  std::size_t n_features = pf_matrix.n_cols;
  std::size_t n_projects = pa_matrix.n_rows;
  std::size_t curr_project;
  std::size_t n_remaining_projects;
  double curr_cost;
  arma::sp_mat::iterator curr_itr;
  arma::sp_mat curr_remaining_actions;
  arma::sp_mat curr_remaining_projects;
  arma::sp_mat curr_remaining_projects2;
  arma::sp_mat initial_actions(1, n_actions);
  arma::sp_mat curr_actions(1, n_actions);
  arma::sp_mat curr_cost_with_project(1, n_projects);
  arma::sp_mat curr_new_actions_per_project(n_projects, n_actions);
  arma::mat curr_feature_shortfalls(1, n_features);
  Progress pb(number_solutions * static_cast<std::size_t>(verbose), verbose);
  bool targets_met;

  // Preliminary processing
  /// initialize cost
  double initial_cost = 0.0;

  // throw error if all actions locked out
  if (static_cast<std::size_t>(locked_out.size()) == n_actions)
    Rcpp::stop("problem infeasible: all actions locked out");

  /// initialize solutions matrix
  Rcpp::LogicalMatrix out(number_solutions, n_actions);
  std::fill(out.begin(), out.end(), FALSE);

  /// initialize remaining action matrix
  arma::sp_mat initial_remaining_actions(1, n_actions);
  for (std::size_t i = 0; i < n_actions; ++i)
    initial_remaining_actions(0, i) = 1.0;

  /// verify targets can be met even if all actions are available
  if (obj_name == "MinimumSetObjective") {
    curr_feature_shortfalls = expected_persistences_shortfalls(
      pa_matrix, pf_matrix, targets, initial_remaining_actions);
    if (curr_feature_shortfalls.max() > 1.0e-10)
      Rcpp::stop("problem infeasible: targets cannot be met");
  }

  /// initialize lock in actions vector
  std::vector<bool> locked_in_actions(n_actions, false);
  for (auto itr = locked_in.begin(); itr != locked_in.end(); ++itr) {
    locked_in_actions[(*itr) - 1] = true;
    initial_remaining_actions.col((*itr) - 1).zeros();
    initial_cost += costs[(*itr) - 1];
  }

  /// lock out actions
  std::vector<bool> locked_out_actions(n_actions, false);
  for (auto itr = locked_out.begin(); itr != locked_out.end();
       ++itr) {
    locked_out_actions[(*itr) - 1] = true;
    initial_remaining_actions.col((*itr) - 1).zeros();
  }

  /// verify that targets can be met given locked out actions
  if (obj_name == "MinimumSetObjective") {
    curr_feature_shortfalls = expected_persistences_shortfalls(
      pa_matrix, pf_matrix, targets, initial_remaining_actions);
    if (curr_feature_shortfalls.max() > 1.0e-10)
      Rcpp::stop("problem infeasible: targets cannot be met given locked out actions");
  }

  /// initialize remaining projects matrix
  arma::sp_mat initial_remaining_projects(1, n_projects);
  for (std::size_t i = 0; i < n_projects; ++i) {
    initial_remaining_projects(0, i) = 1.0;
    for (auto pitr = pa_matrix.begin_row(i); pitr != pa_matrix.end_row(i);
         ++pitr) {
      if ((initial_remaining_actions[pitr.col()] < 0.5) |
           locked_in_actions[pitr.col()]) {
        initial_remaining_projects.col(i).zeros();
        break;
      }
    }
  }

  /// find locked in projects (i.e. projects with all actions locked in)
  std::vector<bool> locked_in_projects(n_projects, true);
  for (std::size_t i = 0; i < n_projects; ++i) {
    for (auto pitr = pa_matrix.begin_row(i); pitr != pa_matrix.end_row(i);
         ++pitr) {
      if (!locked_in_actions[pitr.col()]) {
        locked_in_projects[i] = false;
        break;
      }
    }
  }

  /// find locked out projects (i.e. projects with any actions locked out)
  std::vector<bool> locked_out_projects(n_projects, false);
  for (std::size_t i = 0; i < n_projects; ++i) {
    for (auto pitr = pa_matrix.begin_row(i); pitr != pa_matrix.end_row(i);
         ++pitr) {
      if (locked_out_actions[pitr.col()]) {
        locked_out_projects[i] = true;
        break;
      }
    }
  }

  /// find total cost of each project
  std::vector<double> total_cost_projects(n_projects, 0.0);
  for (std::size_t i = 0; i < n_projects; ++i)
    for (auto pitr = pa_matrix.begin_row(i); pitr != pa_matrix.end_row(i);
         ++pitr)
      total_cost_projects[i] += costs[pitr.col()];

  /// find zero cost projects (i.e. projects comprising actions with no cost)
  /// and remove them from the remaining projects which can be added to the
  /// solutions
  std::vector<bool> zero_cost_projects(n_projects, true);
  for (std::size_t i = 0; i < n_projects; ++i)
    if (total_cost_projects[i] < 1.0e-10)
      initial_remaining_projects.col(i).zeros();

  /// lock out projects which exceed budget
  for (std::size_t i = 0; i < n_projects; ++i) {
    if (total_cost_projects[i] > budget) {
      locked_out_projects[i] = true;
      initial_remaining_projects.col(i).zeros();
    }
  }

  /// remove zero cost actions from the available actions
  for (std::size_t i = 0; i < n_actions; ++i)
    if (costs[i] < 1.0e-15)
      initial_remaining_actions.col(i).zeros();

  /// add locked in actions and zero-cost actions to solutions
  for (std::size_t i = 0; i < n_actions; ++i)
    if ((locked_in_actions[i]) ||
        ((costs[i] < 1.0e-15) && !locked_out_actions[i]))
      for (std::size_t y = 0; y < number_solutions; ++y)
        out(y, i) = TRUE;

  /// set initial actions if min set objective
  if (obj_name == "MinimumSetObjective")
    for (std::size_t i = 0; i < n_actions; ++i)
      if (out(0, i))
        initial_actions(0, i) = 1.0;

  /// verify that starting budget does not exceed budget
  if (initial_cost > budget)
    Rcpp::stop("problem infeasible: locked in actions exceed budget");

  // Main processing
  for (std::size_t y = 0; y < number_solutions; ++y) {

    /// check for user interrupt
    if (y % 1000 == 0)
      if (Progress::check_abort())
        Rcpp::stop("user interupt");

    /// innitialize cost, remaining actions, and targets met
    curr_cost = initial_cost;
    curr_remaining_actions = initial_remaining_actions;
    curr_remaining_projects = initial_remaining_projects;
    n_remaining_projects = initial_remaining_projects.n_nonzero;
    targets_met = false;

    // initialize starting actions if min set objective
    if (obj_name == "MinimumSetObjective")
      curr_actions = initial_actions;

    /// generate random solutions
    while ((std::abs(curr_cost - budget) > 1.0e-10) &&
           (n_remaining_projects > 0) &&
           (!targets_met)) {

      //// reset matrices
      curr_cost_with_project.zeros();
      curr_new_actions_per_project.zeros();

      //// identify which actions associated with each remaining project
      //// are not already funded in the y'th solution
      for (auto ritr = curr_remaining_projects.begin();
           ritr != curr_remaining_projects.end(); ++ritr) {
        for (auto pitr = pa_matrix.begin_row(ritr.col());
             pitr != pa_matrix.end_row(ritr.col()); ++pitr) {
          if ((!out(y, pitr.col())) && (!locked_in_actions[pitr.col()])) {
            curr_new_actions_per_project(ritr.col(), pitr.col()) = 1.0;
          }
        }
      }

      //// calculate cost of funding extra actions required for each project
      for (auto ritr = curr_remaining_projects.begin();
           ritr != curr_remaining_projects.end(); ++ritr)
        for (auto aitr = curr_new_actions_per_project.begin_row(ritr.col());
             aitr != curr_new_actions_per_project.end_row(ritr.col());
             ++aitr)
            curr_cost_with_project[ritr.col()] += costs[aitr.col()];

      /// remove remaining projects associated with exclusive costs that
      /// exceed the budget, and also remove actions exlsuively associated
      /// with these projects too

      curr_remaining_projects2 = curr_remaining_projects;
      for (auto ritr = curr_remaining_projects2.begin();
           ritr != curr_remaining_projects2.end(); ++ritr) {
        if ((curr_cost + curr_cost_with_project[ritr.col()]) > budget) {
          for (auto pitr = curr_new_actions_per_project.begin_row(ritr.col());
               pitr != curr_new_actions_per_project.end_row(ritr.col());
               ++pitr) {
            curr_remaining_actions.col(pitr.col()).zeros();
          }
          curr_remaining_projects.col(ritr.col()).zeros();
        }
      }

      /// if there >= 1 remaining projects after removing those
      /// which would exceed the budget, then randomly add one to the
      /// y'th solution
      n_remaining_projects = curr_remaining_projects.n_nonzero;

      if (n_remaining_projects > 0) {

        /// randomly select project
        curr_project = std::floor(
          R::runif(0.0, static_cast<double>(n_remaining_projects) - 1.0e-15));
        curr_itr = curr_remaining_projects.begin();
        for (std::size_t counter = 0; counter != curr_project; ++counter)
          ++curr_itr;
        curr_project = curr_itr.col();

        /// update the cost
        curr_cost += curr_cost_with_project[curr_project];

        /// remove the selected project from the remaining projects
        curr_remaining_projects.col(curr_project).zeros();
        --n_remaining_projects;

        /// add the excusive actions associated with the project to the
        /// solution
        for (auto aitr = curr_new_actions_per_project.begin_row(curr_project);
             aitr != curr_new_actions_per_project.end_row(curr_project);
             ++aitr)
           out(y, aitr.col()) = TRUE;

         /// remove the exclusive actions from the remaining actions
         for (auto aitr = curr_new_actions_per_project.begin_row(curr_project);
              aitr != curr_new_actions_per_project.end_row(curr_project);
              ++aitr)
            curr_remaining_actions.col(aitr.col()).zeros();

        /// check to see if targets are now met
        if (obj_name == "MinimumSetObjective") {
          for (auto aitr = curr_new_actions_per_project.begin_row(curr_project);
               aitr != curr_new_actions_per_project.end_row(curr_project);
               ++aitr)
            curr_actions(0, aitr.col()) = 1.0;
          curr_feature_shortfalls = expected_persistences_shortfalls(
            pa_matrix, pf_matrix, targets, curr_actions);
          targets_met = !(curr_feature_shortfalls.max() > 0.0);
        }
      } else {
        // stop adding projects for the y'th solution because there are no
        // more projects within the budget
        break;
      }
    }
    //// increment progress bar
    pb.increment();
  }

  // Exports
  return out;
}
