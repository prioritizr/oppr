#' @include Solver-class.R
NULL

#' Add a *Gurobi* solver
#'
#' Add a solver to generate solutions to a project prioritization problem
#' with the [*Gurobi*](https://www.gurobi.com) software.
#' This function can also be used to customize the behavior of the solver.
#' See below for details on installation requirements.
#'
#' @param x [problem()] or [multi_problem()] object.
#'
#' @param gap `numeric` gap to optimality. This gap is relative
#' and expresses the acceptable deviance from the optimal objective.
#' For example, a value of 0.01 will result in the solver stopping when
#' it has found a solution within 1% of optimality.
#' Additionally, a value of 0 will result in the solver stopping
#' when it has found an optimal solution.
#' The default value is 0 (i.e., 0% from optimality).
#'
#' @param number_solutions `integer` number of solutions desired.
#' Defaults to 1. Note that the number of returned solutions can sometimes
#' be less than the argument to `number_solutions` depending on the
#' argument to `solution_pool_method`, for example if 100
#' solutions are requested but only 10 unique solutions exist, then only 10
#' solutions will be returned.
#'
#' @param solution_pool_method `numeric` search method identifier that
#' determines how multiple solutions should be generated. Available search
#' modes for generating a portfolio of solutions include: `0`
#' recording all solutions identified whilst trying to find
#' a solution that is within the specified optimality gap, `1` finding
#' one solution within the optimality gap and a number of additional
#' solutions that are of any level of quality (such that the total number of
#' solutions is equal to `number_solutions`), and `2` finding a
#' specified number of solutions that are nearest to optimality. For more
#' information, see the *Gurobi* manual (i.e., <https://docs.gurobi.com/projects/optimizer/en/current/reference/parameters.html#poolsearchmode>).
#' Defaults to 2.
#'
#' @param time_limit `numeric` time limit in seconds to run the optimizer.
#' The solver will return the current best solution when this time limit is
#' exceeded.
#'
#' @param presolve `integer` number indicating how intensively the
#' solver should try to simplify the problem before solving it. The default
#' value of 2 indicates to that the solver should be very aggressive in
#' trying to simplify the problem.
#'
#' @param threads `integer` number of threads to use for the
#' optimization algorithm. The default value of 1 will result in only
#' one thread being used.
#'
#' @param first_feasible `logical` should the first feasible solution be
#' be returned? If `first_feasible` is set to `TRUE`, the solver
#' will return the first solution it encounters that meets all the
#' constraints, regardless of solution quality. Note that the first feasible
#' solution is not an arbitrary solution, rather it is derived from the
#' relaxed solution, and is therefore often reasonably close to optimality.
#' Defaults to `FALSE`.
#'
#' @param numeric_focus `integer` value denoting how much extra attention be
#' paid to verifying the accuracy of numerical calculations?
#' Acceptable values include 0, 1, 2, or 3.
#' This may be useful when dealing with
#' problems that may suffer from numerical instability issues.
#' Beware that setting greater values will likely increase run time.
#' Defaults to 1.
#'
#' @param start `logical` vector with (`TRUE`/`FALSE`) values for each action
#' indicating if they should be selected by the starting solution.
#' These values should be in the same order of the actions in `x`
#' (i.e., per `action_names(x)`).
#' Missing (`NA`) values can be used to indicate that the solver
#' should automatically calculate starting values for particular actions.
#' Defaults to `NULL` such that starting values are automatically
#' determined by the solver for all actions.
#'
#' @param verbose `logical` should information be printed during optimization?
#' Defaults to `TRUE`.
#'
#' @details
#' [*Gurobi*](https://www.gurobi.com) is a
#' state-of-the-art commercial optimization software with an R package
#' interface. It is by far the fastest of the solvers supported by this
#' package, however, it is also the only solver that is not freely
#' available. That said, licenses are available to academics at no cost. The
#' \pkg{gurobi} package is distributed with the *Gurobi* software suite.
#' This solver uses the \pkg{gurobi} package to solve problems.
#'
#' To install the \pkg{gurobi} package, the
#' [Gurobi](https://www.gurobi.com) optimization suite will first need to
#' be installed (see <https://support.gurobi.com/hc/en-us/articles/4534161999889-How-do-I-install-Gurobi-Optimizer> for instructions). Although
#' [Gurobi](https://www.gurobi.com) is a commercial software, academics
#' can obtain a [special license for no cost](https://www.gurobi.com/downloads/end-user-license-agreement-academic/). After installing the
#' [Gurobi](https://www.gurobi.com) optimization suite, the \pkg{gurobi}
#' package can then be installed (see <https://support.gurobi.com/hc/en-us/articles/14462206790033-How-do-I-install-Gurobi-for-R> for instructions).
#'
#' @return A [problem()] object with the solver added to it.
#'
#' @family solvers
#'
#' @seealso
#' See [solvers] for an overview of functions for adding solvers.
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem
#' p1 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions()
#'
#' # build another problem, and specify the Gurobi solver
#' p2 <- p1 %>% add_gurobi_solver()
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
#' p3 <- p1 %>% add_gurobi_solver(number_solutions = 100)
#'
#' # print problem
#' print(p3)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # print solutions
#' print(s3)
#' @export
add_gurobi_solver <- function(x, gap = 0, number_solutions = 1,
                              solution_pool_method = 2,
                              time_limit = .Machine$integer.max,
                              presolve = 2, threads = 1, first_feasible = FALSE,
                              numeric_focus = 1, start = NULL, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, c("ProjectProblem", "MultiObjProjectProblem")),
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
    assertthat::is.flag(first_feasible),
    assertthat::noNA(first_feasible),
    assertthat::is.number(numeric_focus),
    assertthat::noNA(numeric_focus),
    numeric_focus %in% c(0, 1, 2, 3),
    assertthat::is.flag(verbose),
    requireNamespace("gurobi", quietly = TRUE),
    utils::packageVersion("gurobi") >= package_version("13.0.0")
  )
  if (!is.null(start)) {
    assertthat::assert_that(
      is.logical(start),
      length(start) == x$number_of_actions()
    )
    start <- as.numeric(start)
  }
  # add solver
  x$add_solver(
    R6::R6Class(
      "GurobiSolver",
      inherit = Solver,
      public = list(
        name = "gurobi solver",
        has_pwlobj = TRUE,
        data = list(
          gap = gap,
          number_solutions = number_solutions,
          solution_pool_method = solution_pool_method,
          time_limit = time_limit,
          presolve = presolve,
          threads = threads,
          first_feasible = first_feasible,
          numeric_focus = numeric_focus,
          start = start,
          verbose = verbose
        ),
        solve = function(x, ...) {
          # create problem
          model <- list(
            modelsense = x$modelsense(),
            vtype = x$vtype(),
            obj = x$obj(),
            A = x$A(),
            rhs = x$rhs(),
            sense = x$sense(),
            lb = x$lb(),
            ub = x$ub()
          )
          # add pwl objective if present
          if (length(x$pwlobj()) > 0) {
            model$pwlobj <- x$pwlobj()
          }
          # create parameters
          p <- list(
            LogToConsole = as.numeric(self$get_data("verbose")),
            Presolve = self$get_data("presolve"),
            MIPGap = self$get_data("gap"),
            TimeLimit = self$get_data("time_limit"),
            Threads = self$get_data("threads"),
            LogFile = "",
            ScaleFlag = 2,
            NumericFocus = self$get_data("numeric_focus"),
            SolutionLimit = as.numeric(self$get_data("first_feasible")),
            PoolSolutions = self$get_data("number_solutions"),
            PoolSearchMode = self$get_data("solution_pool_method")
          )
          if (p$SolutionLimit == 0) {
            p$SolutionLimit <- NULL
          }
          # if start solution is available, then use it
          s <- self$get_data("start")
          if (!is.null(s) && !is.Waiver(s) && is.numeric(s)) {
            model$start <- c(s, rep(NA_real_, length(model$obj) - length(s)))
          }
          # solve problem
          rt <- system.time({
            x <- withr::with_locale(
              c(LC_CTYPE = "C"),
              gurobi::gurobi(model = model, params = p)
            )
          })[[3]]
          # round binary variables because default precision is 1e-5
          b <- model$vtype == "B"
          if (is.numeric(x$x)) {
            x$x[b] <- round(x$x[b])
          }
          # extract solution
          out <- list(
            list(
              x = x$x, objective = x$objval, status = x$status, runtime = rt
            )
          )
          # if required, add solutions from solution pool
          if (
            is.numeric(x$x) &&
            isTRUE(length(x$pool) > 1) &&
            isTRUE(self$get_data("number_solutions") > 1)
          ) {
            elem <- ifelse(
              utils::packageVersion("gurobi") >= package_version("13.0.0"),
              "poolnx", "xn"
            )
            out <- append(
              out,
              lapply(x$pool[-1], function(z) {
                list(
                  x = replace(z[[elem]], b, round(z[[elem]][b])),
                  objective = z$objval,
                  status = ifelse(
                    (x$status == "OPTIMAL") &&
                      (abs(x$objval - z$objval) < 1e-5),
                    "OPTIMAL",
                    "SUBOPTIMAL"
                  ),
                  runtime = x$runtime
                )
              })
            )
          }
          # return solution
          out
        }
      )
    )$new()
  )
}
