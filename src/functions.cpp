#include "functions.h"

// Dear reader,
//
// The function definition for `ppp_epd` lives here so that it can
// be accessed via the exported function `rcpp_ppp_epd` and
// also so that it can be cannibalized by `rcpp_heuristic_solution`. The only
// way I could get the package to compile---and without encountering linker
// errors---was to punt the code into its own source file (i.e. this file)
// include a header file (i.e. functions.h) which could be included elsewhere.
// If you have a better idea for doing this, please submit a pull request.

Rcpp::NumericVector ppp_epd(arma::sp_mat spp,
                            arma::sp_mat actions,
                            arma::sp_mat branch_matrix,
                            Rcpp::NumericVector branch_lengths,
                            arma::sp_mat solutions) {
  // Initialization
  std::size_t n_spp = spp.n_cols;
  std::size_t n_projects = spp.n_rows;
  std::size_t n_branches = branch_matrix.n_cols;
  std::size_t n_solutions = solutions.n_rows;
  Rcpp::NumericVector out(n_solutions, 0.0);
  std::vector<std::size_t> curr_funded_projects;
  std::vector<double> curr_solution_best_project_per_species(n_spp);
  double curr_spp_prob;
  double curr_branch_prob;
  bool curr_project_funded;

  // Main processing
  for (std::size_t sol = 0; sol < n_solutions; ++sol) {
    // find out which projects are funded in the current solution
    // i.e. which projects have all of their actions funded
    curr_funded_projects.clear();
    curr_funded_projects.shrink_to_fit();
    curr_funded_projects.reserve(n_projects);
    for (std::size_t p = 0; p < n_projects; ++p) {
      curr_project_funded = true;
      for (auto aitr = actions.begin_row(p);
           aitr != actions.end_row(p);
           ++aitr) {
        if (((*aitr) > 0.5) & (solutions(sol, aitr.col()) < 0.5)) {
          curr_project_funded = false;
          break;
        }
      }
      if (curr_project_funded)
        curr_funded_projects.push_back(p);
    }

    // find best project for each species in the solution
    for (std::size_t s = 0; s < n_spp; ++s) {
      curr_solution_best_project_per_species[s] = 0.0;
      for (auto pitr = curr_funded_projects.cbegin();
           pitr != curr_funded_projects.cend();
           ++pitr) {
          curr_spp_prob = spp(*pitr, s);
        if (curr_spp_prob > curr_solution_best_project_per_species[s])
          curr_solution_best_project_per_species[s] = curr_spp_prob;
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
          (1.0 - curr_solution_best_project_per_species[sitr.row()]);
      }
      out[sol] += (branch_lengths[b] * (1.0 - curr_branch_prob));
    }
  }

  // Exports
  return out;
}
