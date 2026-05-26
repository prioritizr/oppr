#' @include Solver-class.R
NULL

#' Add a *lp_solve* solver with *lpSolveAPI*
#'
#' Add a solver to generate solutions to a project prioritization problem
#' with the *lp_solve* software.
#' This function can also be used to customize the behavior of the
#' solver. It requires the \pkg{lpSolveAPI} package to be installed.
#'
#' @param presolve `logical` indicating if attempts to should be made
#' to simplify the optimization problem (`TRUE`) or not (`FALSE`).
#' Defaults to `TRUE`.
#'
#' @inheritParams add_gurobi_solver
#'
#' @details
#' [*lp_solve*](https://lpsolve.sourceforge.net/5.5/) is an
#' open-source integer programming solver.
#' Although this solver is the slowest currently supported solver,
#' it is also the only exact algorithm solver that can be installed on all
#' operating systems without any manual installation steps. This solver is
#' provided so that users can try solving small project prioritization
#' problems, without needing to install additional software. When solve
#' moderate or large project prioritization problems, consider using
#' [add_gurobi_solver()].
#'
#' @inherit add_gurobi_solver seealso return seealso
#'
#' @family solvers
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with lpSolveAPI solver
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions() %>%
#'   add_lpsolveapi_solver()
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
add_lpsolveapi_solver <- function(x, gap = 0, presolve = FALSE,
                                  verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, c("ProjectProblem", "MultiObjProjectProblem")),
    isTRUE(all(is.finite(gap))),
    assertthat::is.number(gap),
    isTRUE(gap >= 0),
    assertthat::is.flag(presolve),
    assertthat::is.flag(verbose),
    requireNamespace("lpSolveAPI", quietly = TRUE)
  )
  # add solver
  x$add_solver(
    R6::R6Class(
      "LpsolveapiSolver",
      inherit = Solver,
      public = list(
        name = "lpSolveAPI solver",
        data = list(gap = gap, presolve = presolve, verbose = verbose),
        solve = function(x, ...) {
          # assert valid argument
          assertthat::assert_that(
            identical(length(x$pwlobj()), 0L),
            msg = "failed to pre-processs piecewise-linear terms."
          )
          # extract parameters
          p <- as.list(self$data)
          # extract constraints
          m <- as_Matrix(x$A(), "dgTMatrix")
          mrhs <- x$rhs()
          msense <- x$sense()
          # prepare inputs
          l <- lpSolveAPI::make.lp(
            nrow(m), ncol(m),
            ifelse(as.logical(p$verbose), "normal", "neutral")
          )
          lpSolveAPI::name.lp(l, "project prioritization problem")
          for (i in seq_len(ncol(m))) {
            lpSolveAPI::set.column(l, i, m[, i])
          }
          lpSolveAPI::set.objfn(l, x$obj())
          lpSolveAPI::set.rhs(l, mrhs)
          lpSolveAPI::set.constr.type(l, msense, seq_len(nrow(m)))
          lpSolveAPI::set.bounds(l, lower = x$lb(), upper = x$ub())
          v <- x$vtype()
          v[v == "B"] <- "binary"
          v[v == "C"] <- "real"
          v[v == "S"] <- "real"
          v[v == "I"] <- "integer"
          for (i in unique(v)) {
            lpSolveAPI::set.type(l, which(v == i), i)
          }
          s <- which(v == "S")
          if (length(s) > 0) {
            lpSolveAPI::set.semicont(l, s)
          }
          # set parameters
          if (isTRUE(p$presolve)) {
            presolve <- c(
              "rows", "cols", "lindep", "knapsack", "impliedfree",
              "probreduce", "rowdominate", "coldominate", "mergerows",
              "impliedslk", "colfixdual", "duals", "sensduals"
            )
          } else {
            presolve <- "none"
          }
          lpSolveAPI::lp.control(
            l,
            mip.gap = p$gap, presolve = presolve, sense = x$modelsense()
          )
          # solve problem
          rt <- system.time({
            o <- lpSolveAPI::solve.lpExtPtr(l)
          })[[3]]
          # status code
          status <- lp_solve_status(o)
          # check if no solution found
          if (!o %in% c(0, 1, 9, 12)) {
            return(NULL)
          }
          # return solution
          list(
            list(
              x = lpSolveAPI::get.variables(l),
              objective = lpSolveAPI::get.objective(l),
              status = status,
              runtime = rt
            )
          )
        }
      )
    )$new()
  )
}

#' lp_solve status
#'
#' Find a description of the solver status returned from lp_solve.
#'
#' @param x `numeric` status code.
#'
#' @return `character` status description.
#'
#' @noRd
lp_solve_status <- function(x) {
  assertthat::assert_that(is.numeric(x))
  codes <- c(
    "0" = "optimal solution found",
    "1" = "the model is sub-optimal",
    "3" = "the model is unbounded",
    "2" = "the model is infeasible",
    "4" = "the model is degenerate",
    "5" = "numerical failure encountered",
    "6" = "process aborted",
    "7" = "timeout",
    "9" = "the model was solved by presolve",
    "10" = "the branch and bound routine failed",
    "11" = "the branch and bound was stopped because of a break-at-first or break-at-value",
    "12" = "a feasible branch and bound solution was found",
    "13" = "no feasible branch and bound solution was found"
  )
  x <- codes[as.character(x)]
  if (is.na(x)) {
    warning("solver returned unrecognized code")
  }
  as.character(x)
}
