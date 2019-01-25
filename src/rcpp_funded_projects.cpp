#include "package.h"

// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_funded_projects(arma::sp_mat pa_matrix,
                                         arma::sp_mat solutions) {
  // Initialization
  std::size_t n_projects = pa_matrix.n_rows;
  std::size_t n_solutions = solutions.n_rows;
  Rcpp::NumericMatrix out(n_solutions, n_projects);
  std::fill(out.begin(), out.end(), 1.0);

  // Main processing
  for (std::size_t sol = 0; sol < n_solutions; ++sol) {
    for (std::size_t p = 0; p < n_projects; ++p) {
      for (auto aitr = pa_matrix.begin_row(p);
           aitr != pa_matrix.end_row(p);
           ++aitr) {
        if (((*aitr) > 0.5) & (solutions(sol, aitr.col()) < 0.5)) {
          out(sol, p) = 0.0;
          break;
        }
      }
    }
  }

  // Exports
  return out;
}
