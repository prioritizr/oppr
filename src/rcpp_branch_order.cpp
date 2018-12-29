#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_branch_order(arma::sp_mat x) {
  // initialization
  const std::size_t n_cols = x.n_cols;
  /// determine number of features
  std::size_t n_features = 0;
  arma::sp_mat x_col_sums = arma::sum(x, 0);
  arma::sp_mat x_row_sums = arma::sum(x, 1);
  for (std::size_t i = 0; i < x_col_sums.size(); ++i)
    if (x_col_sums[i] < 1.5)
      ++n_features;
  /// output vector
  std::vector<std::size_t> out;
  out.reserve(n_cols);
  
  // main processing
  /// add tip branches
  arma::sp_mat::row_iterator itr;
  for (std::size_t i = 0; i < n_features; ++i) {
    for (auto itr = x.begin_row(i); itr != x.end_row(i); ++itr) {
      if (x_col_sums[itr.col()] < 1.5) {
        out.push_back(itr.col() + 1);
      }
    }
  }

  /// add non-tip branches
  for (std::size_t i = 0; i < n_cols; ++i) {
    if (x_col_sums[i] > 1.5) {
      out.push_back(i + 1);
    }
  }

  // exports
  Rcpp::IntegerVector out2 = wrap(out);
  return out2;
}
