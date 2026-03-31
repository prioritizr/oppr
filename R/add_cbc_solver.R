#' @include Solver-class.R
NULL

#' Add a *CBC* solver
#'
#' Add a solver to generate solutions to a project prioritization problem
#' with the [*CBC*](https://github.com/coin-or/Cbc)
#' (COIN-OR branch and cut) Forrest & Lougee-Heimer 2005).
#' This function can also be used to customize the behavior of the solver.
#' It requires the \pkg{rcbc} package to be installed
#' (only [available on GitHub](https://github.com/dirkschumacher/rcbc),
#' see below for installation instructions).
#'
#' @inheritParams add_gurobi_solver
#'
#' @param presolve `integer` number indicating how intensively the
#'   solver should try to simplify the problem before solving it. Available
#'   options are: (0) disable pre-solving, (1) conservative
#'   level of pre-solving, and (2) very aggressive level of pre-solving .
#'   The default value is 2.
#'
#' @details
#' [*CBC*](https://github.com/coin-or/Cbc) is an
#' open-source mixed integer programming solver that is part of the
#' Computational Infrastructure for Operations Research (COIN-OR) project.
#' This solver seems to have much better performance than the other open-source
#' solvers (i.e., [add_highs_solver()], [add_rsymphony_solver()],
#' [add_lpsymphony_solver()])
#' (see the _Solver benchmarks_ vignette for details).
#' As such, it is strongly recommended to use this solver if the *Gurobi*
#' solver is not available.
#'
#' @section Installation:
#' The \pkg{rcbc} package is required to use this solver. Since the
#' \pkg{rcbc} package is not available on the
#' the Comprehensive R Archive Network (CRAN), it must be installed from
#' [its GitHub repository](https://github.com/dirkschumacher/rcbc). To
#' install the \pkg{rcbc} package, please use the following code:
#' ```
#' if (!require(remotes)) install.packages("remotes")
#' remotes::install_github("dirkschumacher/rcbc")
#' ```
#' Note that you may also need to install several dependencies --
#' such as the
#' [Rtools software](https://cran.r-project.org/bin/windows/Rtools/)
#' or system libraries -- prior to installing the \pkg{rcbc} package.
#' For further details on installing this package, please consult the
#' [online package documentation](https://dirkschumacher.github.io/rcbc/).
#'
#' @inherit add_gurobi_solver return seealso
#'
#' @family solvers
#'
#' @references
#' Forrest J and Lougee-Heimer R (2005) CBC User Guide. In Emerging theory,
#' Methods, and Applications (pp. 257--277). INFORMS, Catonsville, MD.
#' \doi{10.1287/educ.1053.0020}.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create problem
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_cbc_solver(gap = 0, verbose = FALSE)
#'
#' # generate solution %>%
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # create a similar problem with boundary length penalties and
#' # specify the solution from the previous run as a starting solution
#' p2 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_boundary_penalties(10) %>%
#'   add_binary_decisions() %>%
#'   add_cbc_solver(gap = 0, start_solution = s1, verbose = FALSE)
#'
#' # generate solution
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2, main = "solution with boundary penalties", axes = FALSE)
#' }
#' @name add_cbc_solver
NULL

#' @rdname add_cbc_solver
#' @export
add_cbc_solver <- function(x,
                           gap = 0.1,
                           time_limit = .Machine$integer.max,
                           presolve = 2,
                           threads = 1,
                           first_feasible = FALSE,
                           verbose = TRUE) {
  # provide backwards compatibility for presolve
  if (isTRUE(presolve)) {
    presolve <- 1
  }
  if (identical(presolve, FALSE)) {
    presolve <- 0
  }
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, c("ProjectProblem", "MultiObjProjectProblem")),
    assertthat::is.number(gap),
    isTRUE(all(is.finite(gap))),
    gap >= 0,
    assertthat::is.count(time_limit),
    assertthat::noNA(time_limit),
    assertthat::is.number(presolve),
    assertthat::noNA(presolve),
    presolve %in% c(0, 1, 2),
    assertthat::is.count(threads),
    assertthat::is.count(threads),
    assertthat::noNA(threads),
    isTRUE(threads <= parallel::detectCores(TRUE)),
    assertthat::is.flag(first_feasible),
    assertthat::noNA(first_feasible),
    assertthat::is.flag(verbose),
    requireNamespace("rcbc", quietly = TRUE)
  )
  # add solver
  x$add_solver(
    R6::R6Class(
      "CbcSolver",
      inherit = Solver,
      public = list(
        name = "cbc solver",
        data = list(
          gap = gap,
          time_limit = time_limit,
          presolve = presolve,
          threads = threads,
          first_feasible = first_feasible,
          verbose = verbose
        ),
        solve = function(x, ...) {
          # prepare constraints
          ## extract info
          rhs <- x$rhs()
          sense <- x$sense()
          ## initialize CBC arguments
          row_lb <- numeric(length(rhs))
          row_ub <- numeric(length(rhs))
          ## set equality constraints
          idx <- which(sense == "=")
          row_lb[idx] <- rhs[idx]
          row_ub[idx] <- rhs[idx]
          ## set lte constraints
          idx <- which(sense == "<=")
          row_lb[idx] <- -Inf
          row_ub[idx] <- rhs[idx]
          ## set gte constraints
          idx <- which(sense == ">=")
          row_lb[idx] <- rhs[idx]
          row_ub[idx] <- Inf
          # create problem
          model <- list(
            max = identical(x$modelsense(), "max"),
            obj = x$obj(),
            is_integer = x$vtype() == "B",
            is_semi = x$vtype() == "S",
            mat = as_Matrix(x$A(), "dgTMatrix"),
            col_lb = x$lb(),
            col_ub = x$ub(),
            row_lb = row_lb,
            row_ub = row_ub
          )
          # if needed, insert dummy row to ensure non-zero value in last cell
          if (abs(model$mat[nrow(model$mat), ncol(model$mat)]) < 1e-300) {
            model$mat <- as_Matrix(
              rbind(
                model$mat,
                Matrix::sparseMatrix(
                  i = 1, j = ncol(model$mat), x = 1, repr = "T"
                )
              ),
              "dgTMatrix"
            )
            model$row_lb <- c(model$row_lb, -Inf)
            model$row_ub <- c(model$row_ub, Inf)
          }
          # create parameters
          p <- list(
            log = as.character(as.numeric(self$get_data("verbose"))),
            verbose = "1",
            presolve = switch(
              paste0("P", presolve),
              P0 = {"off"},
              P1 = {"on"},
              P2 = {"more"},
              "on"
            ),
            ratio = as.character(self$get_data("gap")),
            sec = as.character(self$get_data("time_limit")),
            threads = as.character(self$get_data("threads")),
            timeMode = "elapsed"
          )
          if (self$get_data("first_feasible") > 0.5) {
            p$maxso <- "1"
          }
          # add starting solution if specified
          start <- self$get_data("start_solution")
          if (!is.null(start) && !is.Waiver(start)) {
            n_extra <- max(length(model$obj) - length(start), 0)
            model$initial_solution <- c(c(start), rep(NA_real_, n_extra))
          }
          # solve problem
          rt <- system.time({
            x <- do.call(rcbc::cbc_solve, append(model, list(cbc_args = p)))
          })
          # return NULL if infeasible
          if (x$is_proven_dual_infeasible ||
              x$is_proven_infeasible ||
              x$is_abandoned) {
            return(NULL)
          }
          # sanitize solver output
          sol <- x$column_solution
          if (is.numeric(x$objective_value)) {
            ## round integer variables
            sol[model$is_integer] <- round(sol[model$is_integer])
            ## truncate variables to account for rounding issues
            sol <- pmax(sol, model$col_lb)
            sol <- pmin(sol, model$col_ub)
          }
          # return solution
          list(
            list(
              x = sol,
              objective = x$objective_value,
              status = as.character(rcbc::solution_status(x)),
              runtime = rt[[3]],
              gap = NA_real_
            )
          )
        }
      )
    )$new()
  )
}
