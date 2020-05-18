#' @include Solver-proto.R
NULL

#' Add a SYMPHONY solver with \pkg{Rsymphony}
#'
#' Specify that the \emph{SYMPHONY} software should be used to solve a
#' project prioritization \code{\link{problem}} using the \pkg{Rsymphony}
#' package. This function can also be used to customize the behavior of the
#' solver. It requires the \pkg{Rsymphony} package.
#'
#' @inheritParams add_gurobi_solver
#'
#' @details \href{https://projects.coin-or.org/SYMPHONY}{\emph{SYMPHONY}} is an
#'   open-source integer programming solver that is part of the Computational
#'   Infrastructure for Operations Research (COIN-OR) project, an initiative
#'   to promote development of open-source tools for operations research (a
#'   field that includes linear programming). The \pkg{Rsymphony} package
#'   provides an interface to COIN-OR and is available on \emph{CRAN}.
#'   This solver uses the \pkg{Rsymphony} package to solve problems.
#'
#' @inherit add_gurobi_solver seealso return
#'
#' @examples
#' \donttest{
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with Rsymphony solver
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions() %>%
#'      add_rsymphony_solver()
#'
#' # print problem
#' print(p)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#'
#' # plot solution
#' plot(p, s)
#' }
#' @name add_rsymphony_solver
NULL

#' @export
#' @rdname Solver-class
methods::setClass("RsymphonySolver", contains = "Solver")

#' @rdname add_rsymphony_solver
#' @export
add_rsymphony_solver <- function(x, gap = 0, time_limit = -1,
                                 first_feasible = 0, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.number(gap),
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.number(time_limit),
                          assertthat::is.count(time_limit) || isTRUE(time_limit
                            == -1),
                          assertthat::is.flag(verbose),
                          assertthat::is.number(first_feasible),
                          isTRUE(first_feasible == 1 || isTRUE(first_feasible
                            == 0)),
                          requireNamespace("Rsymphony", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "RsymphonySolver",
    Solver,
    name = "Rsymphony",
    parameters = parameters(
      numeric_parameter("gap", gap, lower_limit = 0),
      integer_parameter("time_limit", time_limit, lower_limit = -1,
                        upper_limit = .Machine$integer.max),
      binary_parameter("first_feasible", first_feasible),
      binary_parameter("verbose", verbose)),
    solve = function(self, x) {
      assertthat::assert_that(identical(x$pwlobj(), list()),
        msg = "gurobi solver is required to solve problems with this objective")
      model <- list(
        obj = x$obj(),
        mat = as.matrix(x$A()),
        dir = x$sense(),
        rhs = x$rhs(),
        types = x$vtype(),
        bounds = list(lower = list(ind = seq_along(x$lb()), val = x$lb()),
                      upper = list(ind = seq_along(x$ub()), val = x$ub())),
        max = isTRUE(x$modelsense() == "max"))
      p <- as.list(self$parameters)
      p$verbosity <- -1
      if (!p$verbose)
        p$verbosity <- -2
      p <- p[names(p) != "verbose"]
      model$dir <- replace(model$dir, model$dir == "=", "==")
      model$types <- replace(model$types, model$types == "S", "C")
      names(p)[which(names(p) == "gap")] <- "gap_limit"
      p$first_feasible <- as.logical(p$first_feasible)
      start_time <- Sys.time()
      x <- do.call(Rsymphony::Rsymphony_solve_LP, append(model, p))
      end_time <- Sys.time()
      # convert status from integer code to character description
      x$status <- symphony_status(x$status)
      # manually throw infeasible solution if it contains only zeros,
      # this is because during presolve SYMHPONY will incorrectly return
      # a solution with no funded actions when the problem is infeasible
      if (max(x$solution) < 1e-10)
        return(NULL)
      # check if no solution found
      if (is.null(x$solution) ||
          (x$status %in% c("TM_NO_SOLUTION", "PREP_NO_SOLUTION")))
        return(NULL)
      list(list(x = x$solution, objective = x$objval,
                status = as.character(x$status),
                runtime = as.double(end_time - start_time,
                                    format = "seconds")))
    }))
}
