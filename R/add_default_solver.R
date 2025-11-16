#' @include Solver-class.R
NULL

#' Add a default solver
#'
#' Identify the best solver currently installed on the system and specify that
#' it should be used to solve a project prioritization [problem()].
#'
#' @inheritParams add_gurobi_solver
#'
#' @param ... arguments passed to the solver.
#'
#' @details
#' Ranked from best to worst, the solvers that can be used are:
#' \pkg{gurobi}, ([add_gurobi_solver()]),
#' \pkg{highs}, ([add_highs_solver()]),
#' \pkg{Rsymphony} ([add_rsymphony_solver()]), \pkg{lpsymphony}
#' ([add_lpsymphony_solver()]), and \pkg{lpSolveAPI}
#' ([add_lpsolveapi_solver()]). This function does not consider
#' solvers that generate solutions using heuristic algorithms (i.e.
#' [add_heuristic_solver()]) or random processes
#' (i.e. [add_random_solver()]) because they cannot provide
#' any guarantees on solution quality.
#'
#'
#' @inherit add_gurobi_solver return seealso
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with default solver
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#'
#' # print problem
#' print(p)
#'
#' \dontrun{
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#'
#' # plot solution
#' plot(p, s)
#' }
#' @export
add_default_solver <- function(x, ...) {
  ds <- default_solver_name()
  if (identical(ds, "gurobi")) {
    return(add_gurobi_solver(x, ...))
  } else if (identical(ds, "highs")) {
    return(add_highs_solver(x, ...))
  } else if (identical(ds, "Rsymphony")) {
    return(add_rsymphony_solver(x, ...))
  } else if (identical(ds, "lpsymphony")) {
    return(add_lpsymphony_solver(x, ...))
  } else if (identical(ds, "lpSolveAPI")) {
    return(add_lpsolveapi_solver(x, ...))
  } else {
    assertthat::assert_that(
      inherits(x, c("ProjectProblem", "MultiObjProjectProblem"))
    )
    return(
      x$add_solver(
        R6::R6Class(
          "MissingSolver",
          inherit = Solver,
          public = list(
            name = "missing",
            solve = function(x) {
              stop(
                "no optimization problem solvers found on system.",
                call. = FALSE
              )
            }
          )
        )
      )
    )
  }
}

#' Default solver name
#'
#' This function returns the name of the default solver. If no solvers are
#' detected on the system, then a `NULL` object is returned.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{Rsymphony}, \pkg{lpsymphony}, \pkg{gurobi}, \pkg{lpSolveAPI},
#'   and \pkg{highs}.
#'
#' @return `character` indicating the name of the default solver.
#'
#' @noRd
default_solver_name <- function() {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return("gurobi")
  } else if (requireNamespace("highs", quietly = TRUE)) {
    return("highs")
  } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
    return("Rsymphony")
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return("lpsymphony")
  } else {
    return("lpSolveAPI")
  }
}
