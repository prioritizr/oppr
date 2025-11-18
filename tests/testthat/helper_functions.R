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
