context("internal rcpp")

test_that("rcpp_branch_order (regular matrix)", {
  m <- structure(c(1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1,
                   0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0,
                   0, 0, 1), .Dim = c(5L, 8L), .Dimnames = list(NULL, NULL))
  m <- as(m, "dgCMatrix")
  o <- rcpp_branch_order(m)
  expect_equal(o, c(2, 4, 5, 7, 8, 1, 3, 6))
})

test_that("rcpp_branch_order (identity matrix)", {
  m <- diag(5)
  m <- as(m, "dgCMatrix")
  o <- rcpp_branch_order(m)
  expect_equal(o, seq_len(5))
})

test_that("rcpp_branch_order (duplicated features)", {
  m <- cbind(diag(5), diag(5))
  m <- as(m, "dgCMatrix")
  o <- rcpp_branch_order(m)
  expect_equal(o, c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10))
})
