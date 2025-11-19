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
    "Optimal" # highs
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
