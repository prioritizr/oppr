#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::LogicalMatrix rcpp_heuristic_solution(
  Rcpp::NumericVector costs,
  arma::sp_mat pa_matrix,
  arma::sp_mat pf_matrix,
  arma::sp_mat branch_matrix,
  Rcpp::NumericVector branch_lengths,
  Rcpp::NumericVector targets,
  Rcpp::NumericVector weights,
  double budget,
  Rcpp::IntegerVector locked_in,
  Rcpp::IntegerVector locked_out,
  std::size_t number_solutions,
  bool initial_sweep,
  bool verbose,
  const std::string obj_name) {
  // Initialization
  std::size_t n_actions = pa_matrix.n_cols;
  std::size_t n_projects = pa_matrix.n_rows;
  std::size_t n_features = pf_matrix.n_cols;
  double curr_objective;
  double curr_project_greatest_cost;
  double curr_min_value;
  std::vector<double> curr_project_benefit(n_projects);
  arma::mat curr_feature_shortfalls(1, n_features);
  arma::sp_mat curr_pa_sans_project;
  arma::sp_mat curr_pf_sans_project;
  arma::sp_mat curr_pa_matrix = pa_matrix;
  arma::sp_mat curr_pf_matrix = pf_matrix;
  std::vector<double> curr_shared_actions(n_actions);
  std::vector<double> curr_project_costs(n_projects);
  std::size_t curr_iteration = 1;
  std::size_t curr_project;
  double curr_objective_sans_project;
  bool targets_met = true;

  // Preliminary processing
  /// initialize current cost
  double curr_cost = std::accumulate(costs.begin(), costs.end(), 0.0);

  // throw error if all actions locked out
  if (static_cast<std::size_t>(locked_out.size()) == n_actions)
    Rcpp::stop("problem infeasible: all actions locked out");

  //// if minimum set problem objective, overwrite weights to be 1
  if (obj_name == "MinimumSetObjective")
    std::fill(weights.begin(), weights.end(), 1.0);

  /// initialize remaining solution matrix
  arma::sp_mat remaining_actions(1, n_actions);
  for (std::size_t i = 0; i < n_actions; ++i)
    remaining_actions(0, i) = 1.0;

  //// verify that targets can be met
  if (obj_name == "MinimumSetObjective") {
      curr_feature_shortfalls = expected_persistences_shortfalls(
        pa_matrix, pf_matrix, targets, remaining_actions);
      targets_met = !(curr_feature_shortfalls.max() > 0.0);
      if (!targets_met)
        Rcpp::stop("problem infeasible: targets cannot be met");
  }

  /// initialize lock in actions vector
  std::vector<bool> locked_in_actions(n_actions, FALSE);
  double locked_in_cost = 0.0;
  for (auto itr = locked_in.begin(); itr != locked_in.end(); ++itr) {
    locked_in_actions[(*itr) - 1] = TRUE;
    locked_in_cost += costs[(*itr) - 1];
  }

  /// verify that budget can be met given locked in actions
  if (locked_in_cost > budget && (obj_name != "MinimumSetObjective"))
    Rcpp::stop("problem infeasible: budget too low given locked in actions");

  /// lock out actions
  std::vector<bool> locked_out_actions(n_actions, false);
  for (auto itr = locked_out.begin(); itr != locked_out.end(); ++itr) {
    locked_out_actions[(*itr) - 1] = true;
  }

  /// lock out actions which exceed budget
  if (initial_sweep)
    for (std::size_t i = 0; i < n_actions; ++i)
      locked_out_actions[i] = locked_out_actions[i] || (costs[i] > budget);

  /// find total cost of each project
  std::vector<double> total_cost_projects(n_projects, 0.0);
  for (std::size_t i = 0; i < n_projects; ++i)
    for (auto pitr = pa_matrix.begin_row(i); pitr != pa_matrix.end_row(i);
         ++pitr)
      total_cost_projects[i] += costs[pitr.col()];

  /// lock out actions which are only associated with projects that
  /// exceed budget
  bool keep_action;
  if (initial_sweep) {
    for (std::size_t i = 0; i < n_actions; ++i) {
      if (!locked_in_actions[i]) {
        for (auto pitr = pa_matrix.begin_col(i); pitr != pa_matrix.end_col(i);
             ++pitr) {
          keep_action = false;
          if (total_cost_projects[pitr.row()] > budget) {
            keep_action = true;
            break;
          }
          locked_out_actions[i] = locked_out_actions[i] || keep_action;
        }
      }
    }
  }

  /// remove locked out actions from pool of remaining actions,
  /// substract costs for locked out actions,
  /// and adjust the maximum number of iterations
  for (std::size_t i = 0; i < n_actions; ++i) {
    if (locked_out_actions[i]) {
      curr_cost -= costs[i];
      remaining_actions.col(i).zeros();
    }
  }

  //// verify that targets can be met with locked out actions
  if (obj_name == "MinimumSetObjective") {
      curr_feature_shortfalls = expected_persistences_shortfalls(
        pa_matrix, pf_matrix, targets, remaining_actions);
      targets_met = !(curr_feature_shortfalls.max() > 0.0);
      if (!targets_met)
        Rcpp::stop("problem infeasible: targets cannot be met given locked out actions");
  }

  /// find out which projects are remaining
  arma::sp_mat remaining_projects(1, n_projects);
  for (std::size_t j = 0; j < n_projects; ++j) {
    remaining_projects(0, j) = 1.0;
    for (auto aitr = pa_matrix.begin_row(j); aitr != pa_matrix.end_row(j);
         ++aitr) {
      if (remaining_actions(0, aitr.col()) < 0.5) {
        remaining_projects.col(j).zeros();
        break;
      }
    }
  }

  //// remove projects that have actions which are locked out
  for (std::size_t j = 0; j < n_projects; ++j) {
    if (remaining_projects(0, j) < 0.5) {
      pa_matrix.row(j).zeros();
      pf_matrix.row(j).zeros();
      curr_pa_matrix.row(j).zeros();
      curr_pf_matrix.row(j).zeros();
    }
  }

  /// calculate total project costs
  std::vector<double> project_costs(n_projects, 0.0);
  for (std::size_t j = 0; j < n_projects; ++j)
    if (remaining_projects(0, j) > 0.5)
      for (auto aitr = pa_matrix.begin_row(j); aitr != pa_matrix.end_row(j);
           ++aitr)
      project_costs[j] += costs[aitr.col()];

  /// remove projects with costs that exceed budget
  if (initial_sweep) {
    for (std::size_t j = 0; j < n_projects; ++j) {
      if (project_costs[j] > budget) {
        //// remove actions from matrices
        curr_pa_matrix.row(j).zeros();
        curr_pf_matrix.row(j).zeros();
        //// remove actions exclusively associated with the project
        for (auto aitr = pa_matrix.begin_row(j); aitr != pa_matrix.end_row(j);
             ++aitr) {
          if ((arma::accu(curr_pa_matrix.col(aitr.col())) < 0.5) &&
              (!locked_in_actions[aitr.col()])) {
            ///// remove cost
            curr_cost -= costs[aitr.col()];
            //// remove action
            remaining_actions.col(aitr.col()).zeros();
            //// lock out action
            locked_out_actions[aitr.col()] = true;
          }
        }
        //// remove project from remaining projects
        remaining_projects.col(j).zeros();
        pa_matrix.row(j).zeros();
      }
    }
  }

  /// remove actions which are not associated with any remaining projects
  for (std::size_t i = 0; i < n_actions; ++i) {
    if ((arma::accu(pa_matrix.col(i)) < 0.5) &&
        (remaining_actions(0, i) > 0.5) &&
        (!locked_in_actions[i])) {
      pa_matrix.col(i).zeros();
      curr_pa_matrix.col(i).zeros();
      remaining_actions.col(i).zeros();
      locked_out_actions[i] = true;
      curr_cost -= costs[i];
    }
  }

  /// set number of remaining actions
  std::size_t n_remaining_actions = remaining_actions.n_nonzero;

  /// set number of remaining projects
  std::size_t n_remaining_projects = remaining_projects.n_nonzero;

  /// set max iterations as one minus the number of remaining projects
  std::size_t max_iterations = n_remaining_projects;

  /// initialize budget met and find out if it is already met after
  /// deselecting actions
  int budget_met_iteration = -1;
  if ((obj_name != "MinimumSetObjective") &&
      (budget_met_iteration < 0) &&
      ((curr_cost - budget) <= 1.0e-10))
      budget_met_iteration = curr_iteration;

  /// initialize solutions matrix with locked out actions
  Rcpp::LogicalMatrix sols(max_iterations, n_actions);
  for (std::size_t i = 0; i < (max_iterations * n_actions); ++i)
     sols[i] = TRUE;
  for (std::size_t i = 0; i < n_actions; ++i)
    if (locked_out_actions[i])
      for (std::size_t j = 0; j < max_iterations; ++j)
      sols(j, i) = FALSE;

  //// initialize progress bar
  Progress pb(max_iterations * static_cast<std::size_t>(verbose),
              verbose);

  // Main processing
  while (curr_iteration < max_iterations) {

    /// check for user interrupt
    if (curr_iteration % 100 == 0)
      if (Progress::check_abort())
        Rcpp::stop("user interupt");

    /// calculate number of remaining projects which share each action
    for (std::size_t i = 0; i < n_actions; ++i)
      curr_shared_actions[i] = arma::accu(curr_pa_matrix.col(i));

    /// calculate objective with all the remaining actions
    if ((obj_name == "MaximumTargetsMetObjective") ||
        (obj_name == "MinimumSetObjective")) {
      curr_feature_shortfalls = expected_persistences_shortfalls(
        curr_pa_matrix, curr_pf_matrix, targets, remaining_actions);
      curr_objective = 0;
      for (std::size_t f = 0; f < n_features; ++f)
        curr_objective += (-curr_feature_shortfalls[f] * weights[f]);
    } else {
      curr_objective = evaluate_max_phylo_div_objective(
         costs, curr_pa_matrix, curr_pf_matrix, branch_matrix, branch_lengths,
         targets, weights, remaining_actions)[0];
    }

    /// calculate the benefit when each action is removed
    for (std::size_t j = 0; j < n_projects; ++j) {
      if ((remaining_projects[j] > 0.5) &&
          (project_costs[j] > 1.0e-10)) {

        //// calculate cost of project using shared action costs
        //// e.g. if two projects share a $100 action, then each project
        //// incurs a $50 cost for the action
        curr_project_costs[j] = 1.0e-5; // note we add a small value to avoid
                                        // divide by zero issues
        for (auto aitr = curr_pa_matrix.begin_row(j);
             aitr != curr_pa_matrix.end_row(j);
             ++aitr)
          if (!locked_in_actions[aitr.col()])
            curr_project_costs[j] += (costs[aitr.col()] /
                                      curr_shared_actions[aitr.col()]);

        //// create input data with the j'th project removed
        curr_pa_sans_project = curr_pa_matrix;
        curr_pa_sans_project.row(j).zeros();
        curr_pf_sans_project = curr_pf_matrix;
        curr_pf_sans_project.row(j).zeros();

        //// calculate objective with i'th action removed
        if ((obj_name == "MaximumTargetsMetObjective") ||
            (obj_name == "MinimumSetObjective")) {
          ///// calculate shortfallls
          curr_feature_shortfalls = expected_persistences_shortfalls(
            curr_pa_sans_project, curr_pf_sans_project, targets,
            remaining_actions);
          //// if any targets are now unmet, then assign -infinity so they
          //// don't get picked
          if ((obj_name == "MinimumSetObjective") &&
              (curr_feature_shortfalls.max() > 0.0)) {
            curr_objective_sans_project =
              -std::numeric_limits<double>::infinity();
          } else {
            curr_objective_sans_project = 0;
            for (std::size_t f = 0; f < n_features; ++f)
              curr_objective_sans_project += (-curr_feature_shortfalls[f] *
                                              weights[f]);
          }
        } else {
          curr_objective_sans_project = evaluate_max_phylo_div_objective(
             costs, curr_pa_sans_project, curr_pf_sans_project, branch_matrix,
             branch_lengths, targets, weights, remaining_actions)[0];
        }

        //// calculate relative benefit for j'th project
        curr_project_benefit[j] =
          (curr_objective - curr_objective_sans_project) /
          curr_project_costs[j];

      } else if (!(project_costs[j] > 1.0e-10)) {
        // manually assign large, but finite, benefit to actions with zero cost
        curr_project_benefit[j] = std::numeric_limits<double>::max();
        curr_project_costs[j] = 0.0;
      } else {
        // otherwise assign infinite benefit to projects that are already
        // locked out
        curr_project_benefit[j] = std::numeric_limits<double>::infinity();
        curr_project_costs[j] = 0.0;
      }
    }

    // find least cost-effecient value
    curr_min_value = *std::min_element(curr_project_benefit.begin(),
                                       curr_project_benefit.end());

    // find the next project to remove, this is done by finding the project
    // which has a cost-effecient value equal to the worst project,
    // and, to account for ties, also has the most expensive costs in total
    curr_project_greatest_cost = -std::numeric_limits<double>::infinity();
    for (std::size_t j = 0; j < n_projects; ++j) {
      if ((std::abs(curr_project_benefit[j] - curr_min_value) < 1.0e-10) &&
          curr_project_greatest_cost < project_costs[j]) {
        curr_project = j;
        curr_project_greatest_cost = project_costs[j];
      }
    }

    // remove the actions that are unique to the project from the remaining
    // actions, unless they are locked in
    // also update the costs
    for (auto aitr = curr_pa_matrix.begin_row(curr_project);
         aitr != curr_pa_matrix.end_row(curr_project); ++aitr) {
      if ((curr_shared_actions[aitr.col()] < 1.5) &&
          !locked_in_actions[aitr.col()]) {
        remaining_actions.col(aitr.col()).zeros();
        curr_cost -= costs[aitr.col()];
        --n_remaining_actions;
      }
    }

    // remove project from remaining data matrices and counter
    remaining_projects.col(curr_project).zeros();
    curr_pa_matrix.row(curr_project).zeros();
    curr_pf_matrix.row(curr_project).zeros();
    --n_remaining_projects;

    // if solving a minimum set objective,
    // check if targets met and if they are not met then exit main loop
    if (obj_name == "MinimumSetObjective") {
        curr_feature_shortfalls = expected_persistences_shortfalls(
          curr_pa_matrix, curr_pf_matrix, targets, remaining_actions);
      targets_met = !(curr_feature_shortfalls.max() > 0.0);
        if (!targets_met)
          break;
    }

    // update solution
    for (auto aitr = pa_matrix.begin_row(curr_project);
         aitr != pa_matrix.end_row(curr_project); ++aitr)
      if ((curr_shared_actions[aitr.col()] < 1.5) &&
          !locked_in_actions[aitr.col()])
        for (std::size_t k = curr_iteration; k < max_iterations; ++k)
          sols(k, aitr.col()) = FALSE;

    // increment iteration
    ++curr_iteration;

    // check if solving budget limited objective,
    // check if the budget is met and if the number of solutions found
    // is equal to the number of desired solutions then exit main loop
    if ((obj_name != "MinimumSetObjective") &&
        (budget_met_iteration > 0) &&
        ((static_cast<std::size_t>(curr_iteration) - budget_met_iteration) >=
         number_solutions))
      break;

   /// find out if the budget is met after deselecting action
   if ((obj_name != "MinimumSetObjective") &&
       (budget_met_iteration < 0) &&
       ((curr_cost - budget) <= 1.0e-10))
       budget_met_iteration = curr_iteration;

    // increment progress bar
    pb.increment();
  }

  /// verify that at least one solution was found given the budget
  if ((budget_met_iteration == -1) &&
      (obj_name != "MinimumSetObjective") &&
      (max_iterations != 1))
    Rcpp::stop("something went wrong, please file an issue at: https://github.com/prioritizr/oppr/issues.");

  // Exports
  /// subset the solutions to only contain valid solutions
  Rcpp::LogicalMatrix out;
  Rcpp::LogicalMatrix out2(curr_iteration, n_actions);
  std::size_t k;
  if (obj_name == "MinimumSetObjective") {
    /// if min set objective, extract only solutions where targets met
    k = 0;
    for (int i = static_cast<int>(curr_iteration - 1); i >= 0; --i) {
      out2(k, Rcpp::_) = sols(i, Rcpp::_);
      ++k;
    }
    out = out2;
  } else {
    /// otherwise, extract only solutions where budget met
    out = sols(Rcpp::Range(budget_met_iteration - 1, curr_iteration - 1),
               Rcpp::_);
  }

  return out;
}
