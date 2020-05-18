#' @include Solver-proto.R
NULL

#' Add a \pkg{Gurobi} solver
#'
#' Specify that the \emph{Gurobi} software should be used to solve a
#' project prioritization \code{\link{problem}}. This function can also be
#' used to customize the behavior of the solver. In addition to the
#' \emph{Gurobi} software suite, it also requires the \pkg{gurobi} package to
#' be installed.
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param gap \code{numeric} gap to optimality. This gap is relative when
#'   solving problems using \pkg{gurobi}, and will cause the optimizer to
#'   terminate when the difference between the upper and lower objective
#'   function bounds is less than the gap times the upper bound. For example, a
#'   value of 0.01 will result in the optimizer stopping when the difference
#'   between the bounds is 1 percent of the upper bound. For
#'   other solvers, this is the absolute gap, so if the optimal value for
#'   a maximization problem is 10, a gap of 0.01 means that a solution between
#'   10 and 10.01 is required. Defaults to 0, so that optimal solutions will be
#'   returned.
#'
#' @param number_solutions \code{integer} number of solutions desired.
#'   Defaults to 1. Note that the number of returned solutions can sometimes
#'   be less than the argument to \code{number_solutions} depending on the
#'   argument to \code{solution_pool_method}, for example if 100
#'   solutions are requested but only 10 unique solutions exist, then only 10
#'   solutions will be returned.
#'
#' @param solution_pool_method \code{numeric} search method identifier that
#'   determines how multiple solutions should be generated. Available search
#'   modes for generating a portfolio of solutions include: \code{0}
#'   recording all solutions identified whilst trying to find
#'   a solution that is within the specified optimality gap, \code{1} finding
#'   one solution within the optimality gap and a number of additional
#'   solutions that are of any level of quality (such that the total number of
#'   solutions is equal to \code{number_solutions}), and \code{2} finding a
#'   specified number of solutions that are nearest to optimality. For more
#'   information, see the \emph{Gurobi} manual (i.e. \url{http://www.gurobi.com/documentation/8.0/refman/poolsearchmode.html#parameter:PoolSearchMode}). Defaults to 2.
#'
#' @param time_limit \code{numeric} time limit in seconds to run the optimizer.
#'   The solver will return the current best solution when this time limit is
#'   exceeded.
#'
#' @param presolve \code{integer} number indicating how intensively the
#'   solver should try to simplify the problem before solving it. The default
#'   value of 2 indicates to that the solver should be very aggressive in
#'   trying to simplify the problem.
#'
#' @param threads \code{integer} number of threads to use for the
#'   optimization algorithm. The default value of 1 will result in only
#'   one thread being used.
#'
#' @param first_feasible \code{logical} should the first feasible solution be
#'   be returned? If \code{first_feasible} is set to \code{TRUE}, the solver
#'   will return the first solution it encounters that meets all the
#'   constraints, regardless of solution quality. Note that the first feasible
#'   solution is not an arbitrary solution, rather it is derived from the
#'   relaxed solution, and is therefore often reasonably close to optimality.
#'   Defaults to \code{FALSE}.
#'
#' @param verbose \code{logical} should information be printed while solving
#'  optimization problems?
#'
#' @details \href{http://gurobi.com}{\emph{Gurobi}} is a
#'   state-of-the-art commercial optimization software with an R package
#'   interface. It is by far the fastest of the solvers supported by this
#'   package, however, it is also the only solver that is not freely
#'   available. That said, licenses are available to academics at no cost. The
#'   \pkg{gurobi} package is distributed with the \emph{Gurobi} software suite.
#'   This solver uses the \pkg{gurobi} package to solve problems.
#'
#'   To install the \pkg{gurobi} package, the
#'   \href{https://www.gurobi.com}{Gurobi} optimization suite will first need to
#'   be installed (see instructions for \href{http://www.gurobi.com/documentation/8.1/quickstart_linux/software_installation_guid.html}{Linux},
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_mac/software_installation_guid.html}{Mac OSX}, and
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_windows/software_installation_guid.html}{Windows} operating systems). Although
#'   \href{https://www.gurobi.com}{Gurobi} is a commercial software, academics
#'   can obtain a
#'   \href{https://www.gurobi.com/downloads/end-user-license-agreement-academic/}{special license for no cost}. After installing the
#'   \href{https://www.gurobi.com}{Gurobi} optimization suite, the \pkg{gurobi}
#'   package can then be installed (see instructions for \href{http://www.gurobi.com/documentation/8.1/quickstart_linux/r_installing_the_r_package.html}{Linux},
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_mac/r_installing_the_r_package.html}{Mac OSX}, and
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_windows/r_installing_the_r_package.html}{Windows} operating systems).
#'
#' @return \code{\link{ProjectProblem-class}} object with the solver added
#'   to it.
#'
#' @seealso \code{\link{solvers}}.
#'
#' @examples
#' \donttest{
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem
#' p1 <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions()
#'
#' # build another problem, and specify the Gurobi solver
#' p2 <- p1 %>%
#'       add_gurobi_solver()
#'
#' # print problem
#' print(p2)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' plot(p2, s2)
#'
#' # build another problem and obtain multiple solutions
#' # note that this problem doesn't have 100 unique solutions so
#' # the solver won't return 100 solutions
#' p3 <- p1 %>%
#'       add_gurobi_solver(number_solutions = 100)
#'
#' # print problem
#' print(p3)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # print solutions
#' print(s3)
#' }
#' @name add_gurobi_solver
NULL

#' @export
#' @rdname Solver-class
methods::setClass("GurobiSolver", contains = "Solver")

#' @rdname add_gurobi_solver
#' @export
add_gurobi_solver <- function(x, gap = 0, number_solutions = 1,
                              solution_pool_method = 2,
                              time_limit = .Machine$integer.max,
                              presolve = 2, threads = 1, first_feasible = 0,
                              verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          isTRUE(all(is.finite(gap))),
                          assertthat::is.number(gap),
                          assertthat::is.count(number_solutions),
                          assertthat::noNA(number_solutions),
                          assertthat::is.count(solution_pool_method + 1),
                          assertthat::noNA(solution_pool_method),
                          solution_pool_method >= 0,
                          solution_pool_method <= 2,
                          isTRUE(gap >= 0), isTRUE(all(is.finite(time_limit))),
                          assertthat::is.count(time_limit),
                          isTRUE(all(is.finite(presolve))),
                          assertthat::is.count(presolve), isTRUE(presolve <= 2),
                          isTRUE(all(is.finite(threads))),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)),
                          assertthat::is.number(first_feasible),
                          isTRUE(first_feasible == 1 | first_feasible == 0),
                          assertthat::is.flag(verbose),
                          requireNamespace("gurobi", quietly = TRUE))
  # add solver
  x$add_solver(pproto(
    "GurobiSolver",
    Solver,
    name = "Gurobi",
    parameters = parameters(
      numeric_parameter("gap", gap, lower_limit = 0),
      integer_parameter("number_solutions", number_solutions, lower_limit = 0L,
                        upper_limit = as.integer(.Machine$integer.max)),
      integer_parameter("solution_pool_method", solution_pool_method,
                        lower_limit = 0L, upper_limit = 2L),
      integer_parameter("time_limit", time_limit, lower_limit = -1L,
                        upper_limit = as.integer(.Machine$integer.max)),
      integer_parameter("time_limit", time_limit, lower_limit = -1L,
                        upper_limit = as.integer(.Machine$integer.max)),
      integer_parameter("presolve", presolve, lower_limit = 0L,
                        upper_limit = 2L),
      integer_parameter("threads", threads, lower_limit = 1L,
                        upper_limit = parallel::detectCores(TRUE)),
      binary_parameter("first_feasible", first_feasible),
      binary_parameter("verbose", verbose)),
    solve = function(self, x, ...) {
      # create problem
      model <- list(
        modelsense = x$modelsense(),
        vtype = x$vtype(),
        obj = x$obj(),
        A = x$A(),
        rhs = x$rhs(),
        sense = x$sense(),
        lb = x$lb(),
        ub = x$ub())
      # add pwl objective if present
      if (!identical(x$pwlobj(), list()))
        model$pwlobj <- x$pwlobj()
      # create parameters
      p <- list(LogToConsole = as.numeric(self$parameters$get("verbose")),
                Presolve = self$parameters$get("presolve"),
                MIPGap = self$parameters$get("gap"),
                TimeLimit = self$parameters$get("time_limit"),
                Threads = self$parameters$get("threads"),
                LogFile = "",
                NumericFocus = 3,
                SolutionLimit = self$parameters$get("first_feasible"),
                PoolSolutions = self$parameters$get("number_solutions"),
                PoolSearchMode = self$parameters$get("solution_pool_method"))
      if (p$SolutionLimit == 0)
        p$SolutionLimit <- NULL
      # solve problem
      x <- gurobi::gurobi(model = model, params = p)
      # round binary variables because default precision is 1e-5
      b <- model$vtype == "B"
      if (is.numeric(x$x))
        x$x[b] <- round(x$x[b])
      # extract solution
      out <- list(list(x = x$x, objective = x$objval, status = x$status,
                       runtime = x$runtime))
      # add solutions from solution pool if required
      if (is.numeric(x$x) && isTRUE(length(x$pool) > 1) &&
          isTRUE(self$parameters$get("number_solutions") > 1)) {
        out <- append(out,
          lapply(x$pool, function(z)
            list(x = replace(z$xn, b, round(z$xn[b])), objective = z$objval,
                 status = ifelse((x$status == "OPTIMAL") &&
                                 (abs(x$objval - z$objval) < 1e-5),
                                 "OPTIMAL", "SUBOPTIMAL"),
                 runtime = x$runtime)))
      }
      # return solution
      out
    }))
}
