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
  bool verbose,
  const std::string obj_name) {
  // Initialization
  std::size_t n_actions = pa_matrix.n_cols;
  std::size_t n_features = pf_matrix.n_cols;
  double curr_objective;
  std::vector<double> curr_action_benefit(n_actions);
  arma::mat curr_feature_shortfalls(1, n_features);
  arma::sp_mat curr_sans_action;
  std::size_t curr_iteration = 1;
  std::size_t curr_action;
  std::size_t j;
  double curr_objective_sans_action;
  std::size_t max_iterations = n_actions;

  // Preliminary processing
  /// initialize current cost
  double curr_cost = std::accumulate(costs.begin(), costs.end(), 0.0);

  //// if minimum set problem objective, overwrite weights to be 1
  if (obj_name == "MinimumSetObjective")
    std::fill(weights.begin(), weights.end(), 1.0);

  /// initialize remaining solution matrix
  arma::sp_mat remaining_actions(1, n_actions);
  for (std::size_t i = 0; i < n_actions; ++i)
    remaining_actions(0, i) = 1.0;

  /// initialize lock in actions vector
  std::vector<bool> locked_in_vector(n_actions, FALSE);
  for (auto itr = locked_in.begin(); itr != locked_in.end(); ++itr) {
    locked_in_vector[(*itr) - 1] = TRUE;
    --max_iterations;
  }

  /// lock out actions
  for (auto itr = locked_out.begin(); itr != locked_out.end(); ++itr) {
    remaining_actions.col((*itr) - 1).zeros();
    curr_cost -= costs[(*itr) - 1];
    --max_iterations;
  }

  /// if multiple actions with zero cost, then subtract remaining from
  // from max iterations
  bool first_zero_cost = FALSE;
  for (std::size_t i = 0; i < n_actions; ++i) {
    if (costs[i] < 1.0e-15) {
      if (first_zero_cost)
        --max_iterations;
      first_zero_cost = TRUE;
    }
  }

  /// initialize n_remaining actions
  std::size_t n_remaining_actions = remaining_actions.n_nonzero;

  /// initialize solutions matrix with locked out solutions
  Rcpp::LogicalMatrix sols(max_iterations, n_actions);
  for (std::size_t i = 0; i < (max_iterations * n_actions); ++i)
     sols[i] = TRUE;
  for (auto itr = locked_out.begin(); itr != locked_out.end(); ++itr)
    for (std::size_t i = 0; i < max_iterations; ++i)
      sols(i, *itr - 1) = FALSE;

  //// initialize budget met
  int budget_met_iteration = -1;

  //// initialize progress bar
  Progress pb(max_iterations * static_cast<std::size_t>(verbose),
              verbose);

  //// initialize targets met
  bool targets_met = true;
  if (obj_name == "MinimumSetObjective") {
      curr_feature_shortfalls = expected_persistences_shortfalls(
        pa_matrix, pf_matrix, targets, remaining_actions);
      targets_met = !(curr_feature_shortfalls.max() > 0.0);
  }
  if (!targets_met)
    Rcpp::stop("problem infeasible: targets cannot be met given locked out actions");

   /// find out if the budget is met
   if ((obj_name != "MinimumSetObjective") &
       (budget_met_iteration < 0) &
       (curr_cost <= budget))
       budget_met_iteration = curr_iteration;

  // Main processing
  while (curr_iteration < max_iterations) {
    /// check for user interrupt
    if (curr_iteration % 100 == 0)
      if (Progress::check_abort())
        Rcpp::stop("user interupt");

    /// calculate objective with all the remaining actions
    if ((obj_name == "MaximumTargetsMetObjective") ||
        (obj_name == "MinimumSetObjective")) {
      curr_feature_shortfalls = expected_persistences_shortfalls(
        pa_matrix, pf_matrix, targets, remaining_actions);
      curr_objective = 0;
      for (std::size_t f = 0; f < n_features; ++f)
        curr_objective += (-curr_feature_shortfalls[f] * weights[f]);
    } else {
      curr_objective = evaluate_max_phylo_div_objective(
         costs, pa_matrix, pf_matrix, branch_matrix, branch_lengths,
         targets, weights, remaining_actions)[0];
    }

    /// calculate the benefit when each action is removed
    for (std::size_t i = 0; i < n_actions; ++i) {
      if ((remaining_actions[i] > 0.5) &
          (costs[i] > 1e-16) &
          !locked_in_vector[i]) {

        //// create new solution with i'th action removed
        curr_sans_action = remaining_actions;
        curr_sans_action.col(i).zeros();

        //// calculate objective with i'th action removed
        if ((obj_name == "MaximumTargetsMetObjective") ||
            (obj_name == "MinimumSetObjective")) {
          ///// calculate shortfallls
          curr_feature_shortfalls = expected_persistences_shortfalls(
            pa_matrix, pf_matrix, targets, curr_sans_action);
          //// if any targets are now unmet, then assign -infinity so they
          //// don't get picked
          if ((obj_name == "MinimumSetObjective") &
              (curr_feature_shortfalls.max() > 0.0)) {
            curr_objective_sans_action =
              -std::numeric_limits<double>::infinity();
          } else {
            curr_objective_sans_action = 0;
            for (std::size_t f = 0; f < n_features; ++f)
              curr_objective_sans_action += (-curr_feature_shortfalls[f] *
                                             weights[f]);
          }
        } else {
          curr_objective_sans_action = evaluate_max_phylo_div_objective(
             costs, pa_matrix, pf_matrix, branch_matrix, branch_lengths,
             targets, weights, curr_sans_action)[0];
        }

        //// calculate relative benefit for i'th action
        curr_action_benefit[i] = (curr_objective -
                                  curr_objective_sans_action) / costs[i];

      } else if (!(costs[i] > 1e-16) &
                 !locked_in_vector[i]) {
        // manually assign large, but finite, benefit to actions with zero cost
        curr_action_benefit[i] = std::numeric_limits<double>::max();
      } else {
        // manually assign infinite benefit to locked in actions,
        // this way zero cost actions are removed before locked in actions
        curr_action_benefit[i] = std::numeric_limits<double>::infinity();
      }
    }

    // find the next selected action
    curr_action = std::distance(curr_action_benefit.begin(),
                                std::min_element(curr_action_benefit.begin(),
                                                 curr_action_benefit.end()));

    // update the cost
    curr_cost -= costs[curr_action];

    // remove the selected action from the remaining actions
    remaining_actions.col(curr_action).zeros();

    // if solving a minimum set objective,
    // check if targets met and if they are not met then exit main loop
    if (obj_name == "MinimumSetObjective") {
        curr_feature_shortfalls = expected_persistences_shortfalls(
          pa_matrix, pf_matrix, targets, remaining_actions);
      targets_met = !(curr_feature_shortfalls.max() > 0.0);
        if (!targets_met)
          break;
    }

    // update solution
    for (std::size_t j = curr_iteration; j < max_iterations; ++j)
      sols(j, curr_action) = FALSE;

    // increment counters
    --n_remaining_actions;
    ++curr_iteration;

    // check if solving budget limited objective,
    // check if the budget is met and if the number of solutions found
    // is equal to the number of desired solutions then exit main loop
    if ((obj_name != "MinimumSetObjective") &
        (budget_met_iteration > 0) &
        ((static_cast<std::size_t>(curr_iteration) - budget_met_iteration) >=
         number_solutions))
      break;

   /// find out if the budget is met
   if ((obj_name != "MinimumSetObjective") &
       (budget_met_iteration < 0) &
       (curr_cost <= budget))
       budget_met_iteration = curr_iteration;

    // increment progress bar
    pb.increment();
  }

  // Exports
  if ((obj_name != "MinimumSetObjective") & (budget_met_iteration < 0))
    Rcpp::stop("problem infeasible: budget too low given locked in actions");

  // subset the solutions to only contain valid solutions
  Rcpp::LogicalMatrix out;
  Rcpp::LogicalMatrix out2(curr_iteration, n_actions);
  if (obj_name == "MinimumSetObjective") {
    // if min set objective, extract only solutions where targets met
    j = 0;
    for (int i = static_cast<int>(curr_iteration - 1); i >= 0; --i) {
      out2(j, Rcpp::_) = sols(i, Rcpp::_);
      ++j;
    }
    out = out2;
  } else {
    // otherwise, extract only solutions where budget met
    out = sols(Rcpp::Range(budget_met_iteration - 1, curr_iteration - 1),
               Rcpp::_);
  }

  return out;
}
