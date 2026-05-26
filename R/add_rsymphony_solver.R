#' @include Solver-class.R
NULL

#' Add a *SYMPHONY* solver with *Rsymphony*
#'
#' Add a solver to generate solutions to a project prioritization problem
#' with the *SYMPHONY* software via the \pkg{Rsymphony} package.
#' This function can also be used to customize the behavior of the
#' solver. It requires the \pkg{Rsymphony} package to be installed.
#'
#' @inheritParams add_gurobi_solver
#'
#' @details
#' [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) is an
#' open-source integer programming solver that is part of the Computational
#' Infrastructure for Operations Research (COIN-OR) project, an initiative
#' to promote development of open-source tools for operations research (a
#' field that includes linear programming). The \pkg{Rsymphony} package
#' provides an interface to COIN-OR and is available on *CRAN*.
#' This solver uses the \pkg{Rsymphony} package to solve problems.
#'
#' @inherit add_gurobi_solver seealso return
#'
#' @family solvers
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with Rsymphony solver
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions() %>%
#'   add_rsymphony_solver()
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
#' @export
add_rsymphony_solver <- function(x, gap = 0, time_limit = .Machine$integer.max,
                                 first_feasible = FALSE, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, c("ProjectProblem", "MultiObjProjectProblem")),
    assertthat::is.number(gap),
    isTRUE(all(is.finite(gap))),
    isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
    assertthat::is.number(time_limit),
    assertthat::is.count(time_limit) ||
      isTRUE(time_limit == -1),
    assertthat::is.flag(verbose),
    assertthat::is.flag(first_feasible),
    requireNamespace("Rsymphony", quietly = TRUE)
  )
  # add solver
  x$add_solver(
    R6::R6Class(
      "RsymphonySolver",
      inherit = Solver,
      public = list(
        name = "rsymphony solver",
        data = list(
          gap = gap,
          time_limit = time_limit,
          first_feasible = first_feasible,
          verbose = verbose
        ),
        solve = function(x, ...) {
          # assert valid argument
          assertthat::assert_that(
            identical(length(x$pwlobj()), 0L),
            msg = "failed to pre-processs piecewise-linear terms."
          )
          # build model
          model <- list(
            obj = x$obj(),
            mat = as.matrix(x$A()),
            dir = x$sense(),
            rhs = x$rhs(),
            types = x$vtype(),
            bounds = list(
              lower = list(ind = seq_along(x$lb()), val = x$lb()),
              upper = list(ind = seq_along(x$ub()), val = x$ub())
            ),
            max = isTRUE(x$modelsense() == "max")
          )
          model$dir <- replace(model$dir, model$dir == "=", "==")
          assertthat::assert_that(
            !any(model$types == "S"),
            msg =
              "`add_rsymphony_solver()` is not compatible with this objective."
          )
          # build parameters
          p <- as.list(self$data)
          p$verbosity <- -1
          if (!p$verbose) {
            p$verbosity <- -2
          }
          p <- p[names(p) != "verbose"]
          names(p)[which(names(p) == "gap")] <- "gap_limit"
          p$first_feasible <- as.logical(p$first_feasible)
          # generate solution
          rt <- system.time({
            x <- do.call(Rsymphony::Rsymphony_solve_LP, append(model, p))
          })[[3]]
          # convert status from integer code to character description
          x$status <- symphony_status(x$status)
          # manually throw infeasible solution if it contains only zeros,
          # this is because during presolve SYMHPONY will incorrectly return
          # a solution with no funded actions when the problem is infeasible
          if (max(x$solution) < 1e-10) {
            return(NULL)
          }
          # check if no solution found
          if (
            is.null(x$solution) ||
              (x$status %in% c("TM_NO_SOLUTION", "PREP_NO_SOLUTION"))
          ) {
            return(NULL) # nocov
          }
          # return solution
          list(
            list(
              x = x$solution,
              objective = x$objval,
              status = as.character(x$status),
              runtime = rt
            )
          )
        }
      )
    )$new()
  )
}

#' SYMPHONY status
#'
#' Find a description of the solver status returned from SYMPHONY.
#'
#' @param x `numeric` status code.
#'
#' @return `character` status description.
#'
#' @noRd
symphony_status <- function(x) {
  assertthat::assert_that(is.numeric(x))
  codes <- c(
    "0" = "TM_OPTIMAL_SOLUTION_FOUND",
    "225" = "TM_NO_PROBLEM",
    "226" = "TM_NO_SOLUTION",
    "227" = "TM_OPTIMAL_SOLUTION_FOUND",
    "228" = "TM_TIME_LIMIT_EXCEEDED",
    "229" = "TM_NODE_LIMIT_EXCEEDED",
    "230" = "TM_ITERATION_LIMIT_EXCEEDED",
    "231" = "TM_TARGET_GAP_ACHIEVED",
    "232" = "TM_FOUND_FIRST_FEASIBLE",
    "233" = "TM_FINISHED",
    "234" = "TM_UNFINISHED",
    "235" = "TM_FEASIBLE_SOLUTION_FOUND",
    "236" = "TM_SIGNAL_CAUGHT",
    "237" = "TM_UNBOUNDED",
    "238" = "PREP_OPTIMAL_SOLUTION_FOUND",
    "239" = "PREP_NO_SOLUTION",
    "-250" = "TM_ERROR__NO_BRANCHING_CANDIDATE",
    "-251" = "TM_ERROR__ILLEGAL_RETURN_CODE",
    "-252" = "TM_ERROR__NUMERICAL_INSTABILITY",
    "-253" = "TM_ERROR__COMM_ERROR",
    "-275" = "TM_ERROR__USER",
    "-276" = "PREP_ERROR"
  )
  x <- codes[as.character(x)]
  if (is.na(x)) {
    warning("solver returned unrecognized code")
  }
  as.character(x)
}
