is_optimal_solver_status <- function(x) {
  # assert argument is valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x)
  )
  # define solver status values that indicate optimality
  status <- c(
    "OPTIMAL", # gurobi
    "TM_OPTIMAL_SOLUTION_FOUND", # SYMPHONY
    "optimal solution found", # lpsolveapi
    "Optimal", # highs
    "optimal" # cbc
  )
  # check if x is an optimal solver status
  isTRUE(x %in% status)
}

gsub_column <- function(x, name, pattern, replacement) {
  assertthat::assert_that(
    is.data.frame(x),
    assertthat::is.string(name),
    assertthat::noNA(name),
    assertthat::has_name(x, name),
    assertthat::is.string(pattern),
    assertthat::noNA(pattern),
    assertthat::is.string(replacement),
    assertthat::noNA(replacement)
  )
  x[[name]] <- gsub(pattern, replacement, x[[name]])
  x
}

gsub_names <- function(x, pattern, replacement) {
  assertthat::assert_that(
    is.data.frame(x),
    assertthat::is.string(pattern),
    assertthat::noNA(pattern),
    assertthat::is.string(replacement),
    assertthat::noNA(replacement)
  )
  stats::setNames(
    x,
    gsub(pattern, replacement, names(x))
  )
}

reorder_matrix <- function(x, y) {
  assertthat::assert_that(
    inherits(x, "dgCMatrix"),
    inherits(y, "dgCMatrix"),
    identical(dim(x), dim(y))
  )
  out <- matrix(NA, ncol = ncol(x), nrow = nrow(y))
  for (i in seq_len(nrow(y))) {
    xdists <- Matrix::rowSums(abs(y[rep(i, nrow(x)), , drop = FALSE] - x))
    xidx <- which(xdists == 0)
    if (length(xidx) == 0) {
      out[i, ] <- x[i, ]
    } else {
      out[i, ] <- x[xidx[[1]], ]
    }
  }
  as_Matrix(out, "dgCMatrix")
}
