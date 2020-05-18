#' @include Solver-proto.R
NULL

#' Add a lp_solve solver with \pkg{lpSolveAPI}
#'
#' Specify that the \emph{lp_solve} software should be used to solve a
#' project prioritization \code{\link{problem}} using the \pkg{lpSolveAPI}
#' package. This function can also be used to customize the behavior of the
#' solver. It requires the \pkg{lpSolveAPI} package.
#'
#' @param presolve \code{logical} indicating if attempts to should be made
#'   to simplify the optimization problem (\code{TRUE}) or not (\code{FALSE}).
#'   Defaults to \code{TRUE}.
#'
#' @inheritParams add_gurobi_solver
#'
#' @details \href{http://lpsolve.sourceforge.net/5.5/}{\emph{lp_solve}} is an
#'   open-source integer programming solver.
#'   Although this solver is the slowest currently supported solver,
#'   it is also the only exact algorithm solver that can be installed on all
#'   operating systems without any manual installation steps. This solver is
#'   provided so that
#'   users can try solving small project prioritization problems, without
#'   needing to install additional software. When solve moderate or large
#'   project prioritization problems, consider using
#'   \code{\link{add_gurobi_solver}}.
#'
#' @inherit add_gurobi_solver seealso return
#'
#' @seealso \code{\link{solvers}}.
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with lpSolveAPI solver
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions() %>%
#'      add_lpsolveapi_solver()
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
#' @name add_lpsolveapi_solver
NULL

#' @export
#' @rdname Solver-class
methods::setClass("LpsolveapiSolver", contains = "Solver")

#' @rdname add_lpsolveapi_solver
#' @export
add_lpsolveapi_solver <- function(x, gap = 0, presolve = FALSE, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.number(gap),
                          isTRUE(gap >= 0),
                          assertthat::is.flag(presolve),
                          assertthat::is.flag(verbose),
                          requireNamespace("lpSolveAPI", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "LpsolveapiSolver",
    Solver,
    name = "lpSolveAPI",
    parameters = parameters(
      numeric_parameter("gap", gap, lower_limit = 0),
      binary_parameter("presolve", presolve),
      binary_parameter("verbose", verbose)),
    solve = function(self, x) {
      assertthat::assert_that(identical(x$pwlobj(), list()),
        msg = "gurobi solver is required to solve problems with this objective")
      # extract parameters
      p <- as.list(self$parameters)
      # extract constraints
      m <- methods::as(x$A(), "dgTMatrix")
      mrhs <- x$rhs()
      msense <- x$sense()
      # manually add in locked constraints
      locked_in <- which(x$lb() > 0.5)
      locked_out <- which(x$ub() < 0.5)
      n_locked <- length(locked_in) + length(locked_out)
      if (n_locked > 0) {
        mrhs <- c(mrhs, rep(1, length(locked_in) ), rep(0, length(locked_out)))
        msense <- c(msense, rep("=", length(n_locked)))
        m2 <- Matrix::sparseMatrix(i = seq_len(n_locked),
                                   j = c(locked_in, locked_out),
                                   x = rep(1, n_locked),
                                   dims = list(n_locked, ncol(m)))
        m <- rbind(m, m2)
      }
      # prepare inputs
      l <- lpSolveAPI::make.lp(nrow(m), ncol(m),
                               ifelse(as.logical(p$verbose), "normal",
                                      "neutral"))
      lpSolveAPI::name.lp(l, "project prioritization problem")
      for (i in seq_len(ncol(m)))
        lpSolveAPI::set.column(l, i, m[, i])
      lpSolveAPI::set.objfn(l, x$obj())
      lpSolveAPI::set.rhs(l, mrhs)
      lpSolveAPI::set.constr.type(l, msense, seq_len(nrow(m)))
      lpSolveAPI::set.bounds(l, lower = x$lb(), upper = x$ub())
      v <- x$vtype()
      s <- which(v == "S")
      v[v == "B"] <- "binary"
      v[v == "C"] <- "real"
      v[v == "S"] <- "real"
      v[v == "I"] <- "integer"
      for (i in unique(v))
        lpSolveAPI::set.type(l, which(v == i), i)
      if (length(s) > 0)
        lpSolveAPI::set.semicont(l, s)
      # set parameters
      if (isTRUE(p$presolve)) {
        presolve <- c("rows", "cols", "lindep", "knapsack", "impliedfree",
                      "probreduce", "rowdominate", "coldominate", "mergerows",
                      "impliedslk", "colfixdual", "duals", "sensduals")
      } else {
        presolve <- "none"
      }
      lpSolveAPI::lp.control(l, mip.gap = p$gap, presolve = presolve,
                             sense = x$modelsense())
      # solve problem
      start_time <- Sys.time()
      o <- lpSolveAPI::solve.lpExtPtr(l)
      end_time <- Sys.time()
      # status code
      status <- lp_solve_status(o)
      # check if no solution found
      if (!o %in% c(0, 1, 9, 12))
        return(NULL)
      # return solution
      list(list(x = lpSolveAPI::get.variables(l),
                objective = lpSolveAPI::get.objective(l),
                status = status,
                runtime = as.double(end_time - start_time,
                                    format = "seconds")))
    }))
}
