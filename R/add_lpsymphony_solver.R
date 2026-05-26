#' @include Solver-class.R
NULL

#' Add a *SYMPHONY* solver with *lpsymphony*
#'
#' Add a solver to generate solutions to a project prioritization problem
#' with the *SYMPHONY* software. This function can also be used to customize
#' the behavior of the solver.
#' It requires the \pkg{lpsymphony} package to be installed.
#'
#' @inheritParams add_gurobi_solver
#'
#' @details
#' [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) is an
#' open-source integer programming solver that is part of the Computational
#' Infrastructure for Operations Research (COIN-OR) project, an initiative
#' to promote development of open-source tools for operations research (a
#' field that includes linear programming). The \pkg{lpsymphony} package is
#' distributed through
#' [Bioconductor](https://doi.org/doi:10.18129/B9.bioc.lpsymphony).
#' This functionality is provided because the \pkg{lpsymphony} package may
#' be easier to install to install on Windows and Mac OSX systems than the
#' \pkg{Rsymphony} package.
#'
#' @inherit add_gurobi_solver seealso return
#'
#' @family solvers
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with lpsymphony solver
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions() %>%
#'   add_lpsymphony_solver()
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
add_lpsymphony_solver <- function(x, gap = 0, time_limit = .Machine$integer.max,
                                  first_feasible = FALSE, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, c("ProjectProblem", "MultiObjProjectProblem")),
    isTRUE(all(is.finite(gap))),
    assertthat::is.number(gap),
    isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
    assertthat::is.number(time_limit),
    assertthat::is.count(time_limit) ||
      isTRUE(time_limit == -1),
    assertthat::is.flag(verbose),
    assertthat::is.flag(first_feasible),
    requireNamespace("lpsymphony", quietly = TRUE)
  )
  # throw warning about bug in lpsymphony
  if (utils::packageVersion("lpsymphony") <= as.package_version("1.4.1")) {
    warning(
      paste(
        "The solution may be incorrect due to a bug in",
        "lpsymphony. Please verify that it is correct,",
        "or use a different solver to generate solutions."
      ),
      call. = FALSE, immediate. = TRUE
    )
  }
  # add solver
  x$add_solver(
    R6::R6Class(
      "LpsymphonySolver",
      inherit = Solver,
      public = list(
        name = "lpsymphony solver",
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
          # prepare model
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
              "`add_lpsymphony_solver()` is not compatible with this objective."
          )
          # prepare parameters
          p <- self$data
          if (!isTRUE(p$verbose)) {
            p$verbosity <- -2
          }
          p <- p[names(p) != "verbose"]
          names(p)[which(names(p) == "gap")] <- "gap_limit"
          # generate solution
          rt <- system.time({
            x <- do.call(lpsymphony::lpsymphony_solve_LP, append(model, p))
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
            return(NULL)
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
