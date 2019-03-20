#include "functions.h"

// calculate euclidean distance
double distance(double x0, double y0, double x1, double y1) {
  return(sqrt(std::abs(Pow<2>(x0-x1)) + std::abs(Pow<2>(y0-y1))));
}

// calculate phylogenetic diversity objective values
Rcpp::NumericVector evaluate_max_phylo_div_objective(
  Rcpp::NumericVector costs, arma::sp_mat pa_matrix, arma::sp_mat pf_matrix,
  arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths,
  Rcpp::NumericVector targets, Rcpp::NumericVector weights,
  arma::sp_mat solutions) {
    arma::mat p = expected_persistences(pa_matrix, pf_matrix,
                                        branch_matrix,
                                        solutions);
    Rcpp::NumericVector out(solutions.n_rows, 0.0);
    for (std::size_t i = 0; i < p.n_rows; ++i)
      for (std::size_t j = 0; j < p.n_cols; ++j)
        out[i] += p(i, j) * branch_lengths[j];
    for (std::size_t j = 0; j < pf_matrix.n_cols; ++j)
      for (std::size_t i = 0; i < p.n_rows; ++i)
         out[i] += p(i, j) * weights[j];
    return out;
}

// calculate max targets objective values
Rcpp::NumericVector evaluate_max_targets_met_objective(
  Rcpp::NumericVector costs, arma::sp_mat pa_matrix, arma::sp_mat pf_matrix,
  arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths,
  Rcpp::NumericVector targets, Rcpp::NumericVector weights,
  arma::sp_mat solutions) {
    arma::mat p = expected_persistences(pa_matrix, pf_matrix, branch_matrix,
                                        solutions);
    Rcpp::NumericVector out(solutions.n_rows, 0.0);
    for (std::size_t j = 0; j < p.n_cols; ++j)
      for (std::size_t i = 0; i < p.n_rows; ++i)
        out[i] += (static_cast<double>(p(i, j) >= targets[j]) * weights[j]);
    return out;
}

// calculate min set objective values
Rcpp::NumericVector evaluate_min_set_objective(
  Rcpp::NumericVector costs, arma::sp_mat pa_matrix, arma::sp_mat pf_matrix,
  arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths,
  Rcpp::NumericVector targets, Rcpp::NumericVector weights,
  arma::sp_mat solutions) {
    std::size_t n_actions = costs.size();
    std::size_t n_solutions = solutions.n_rows;
    Rcpp::NumericVector out(solutions.n_rows, 0.0);
    for (std::size_t i = 0; i < n_actions; ++i)
      for (std::size_t j = 0; j < n_solutions; ++j)
        out[j] += (solutions(j, i) * costs[i]);
    return out;
}

// calculate shortfall in expected persistences
arma::mat expected_persistences_shortfalls(
  arma::sp_mat pa_matrix, arma::sp_mat pf_matrix, Rcpp::NumericVector targets,
  arma::sp_mat solutions) {
  // create dummy branch matrix
  arma::sp_mat bm(pf_matrix.n_cols, pf_matrix.n_cols);
  bm.eye();
  // calculate persistences
  arma::mat p = expected_persistences(pa_matrix, pf_matrix, bm, solutions);
  // calculate shortfall for each feature
  arma::mat out(solutions.n_rows, pf_matrix.n_cols);
  for (std::size_t f = 0; f < p.n_cols; ++f)
    for (std::size_t i = 0; i < solutions.n_rows; ++i)
      out(i, f) = targets[f] - p(i, f);
  // return shortfall
  return out;
}

// calculate expected persistence
arma::mat expected_persistences(
  arma::sp_mat pa_matrix, arma::sp_mat pf_matrix, arma::sp_mat branch_matrix,
  arma::sp_mat solutions) {
  // Initialization
  std::size_t n_features = pf_matrix.n_cols;
  std::size_t n_projects = pf_matrix.n_rows;
  std::size_t n_branches = branch_matrix.n_cols;
  std::size_t n_solutions = solutions.n_rows;
  std::vector<std::size_t> curr_funded_projects;
  std::vector<double> curr_solution_best_project_per_feature(n_features);
  double curr_feature_prob;
  double curr_branch_prob;
  bool curr_project_funded;
  arma::mat out(n_solutions, branch_matrix.n_cols);

  // Main processing
  for (std::size_t sol = 0; sol < n_solutions; ++sol) {
    // find out which projects are funded in the current solution
    // i.e. which projects have all of their pa_matrix funded
    curr_funded_projects.clear();
    curr_funded_projects.shrink_to_fit();
    curr_funded_projects.reserve(n_projects);
    for (std::size_t p = 0; p < n_projects; ++p) {
      curr_project_funded = true;
      for (auto aitr = pa_matrix.begin_row(p);
           aitr != pa_matrix.end_row(p);
           ++aitr) {
        if (((*aitr) > 0.5) & (solutions(sol, aitr.col()) < 0.5)) {
          curr_project_funded = false;
          break;
        }
      }
      if (curr_project_funded)
        curr_funded_projects.push_back(p);
    }

    // find best project for each feature in the solution
    for (std::size_t f = 0; f < n_features; ++f) {
      curr_solution_best_project_per_feature[f] = 0.0;
      for (auto pitr = curr_funded_projects.cbegin();
           pitr != curr_funded_projects.cend();
           ++pitr) {
          curr_feature_prob = pf_matrix(*pitr, f);
        if (curr_feature_prob > curr_solution_best_project_per_feature[f])
          curr_solution_best_project_per_feature[f] = curr_feature_prob;
      }
    }

    // iterate over each branch and calculate the expected amount of
    // evolutionary history that is conserved given each solution
    for (std::size_t b = 0; b < n_branches; ++b) {
      curr_branch_prob = 1.0;
      for (auto sitr = branch_matrix.begin_col(b);
           sitr != branch_matrix.end_col(b);
           ++sitr) {
        curr_branch_prob *=
          (1.0 - curr_solution_best_project_per_feature[sitr.row()]);
      }
      out(sol, b) = (1.0 - curr_branch_prob);
    }
  }

  // Exports
  return out;
}
