#' @include Solver-proto.R
NULL

#' Add a random solver
#'
#' Specify that solutions should be generated using random processes. Although
#' prioritizations should be developed using optimization routines,
#' a portfolio of randomly generated solutions can be useful for evaluating
#' the effectiveness of solutions.
#'
#' @inheritParams add_gurobi_solver
#'
#' @details The algorithm used to randomly generate solutions depends on the
#'  the objective specified for the project prioritization
#'  \code{\link{problem}}.
#'
#'  For objectives which maximize benefit subject to budgetary constraints
#'  (e.g. \code{\link{add_max_sum_persistence_objective}}):
#'
#'  \enumerate{
#'
#'  \item All actions are initially selected for funding (excepting actions
#'    which are locked out).
#'
#'  \item An action is randomly deselected for funding (excepting actions which
#'    are locked in or actions which have zero cost).
#'
#' \item The previous step is repeated until the total cost of the remaining
#'   actions that are prioritized for funding falls within the budget.
#'
#'  }
#'
#' For objectives which minimize cost subject to biodiversity constraints
#' (i.e. \code{\link{add_min_set_objective}}:
#'
#'  \enumerate{
#'
#'  \item All actions are initially not selected for funding (excepting actions
#'    which are locked in or actions which have zero cost).
#'
#'  \item An action is randomly selected for funding (excepting actions which
#'    are locked out.
#'
#' \item The previous step is repeated until all of the persistence targets
#'   are met.
#'
#'  }
#'
#' @inherit add_gurobi_solver seealso return
#'
#' @examples
#' #TODO
#'
#' @name add_random_solver
NULL

#' @export
methods::setClass("RandomSolver", contains = "Solver")

#' @rdname add_random_solver
#' @export
add_random_solver <- function(x, number_solutions = 1, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          assertthat::is.count(number_solutions),
                          assertthat::noNA(number_solutions),
                          assertthat::is.flag(verbose),
                          assertthat::noNA(verbose))
  # add solver
  x$add_solver(pproto(
    "RandomSolver",
    Solver,
    name = "Random",
    parameters = parameters(
      integer_parameter("number_solutions", number_solutions, lower_limit = 0L,
                        upper_limit = as.integer(.Machine$integer.max)),
      binary_parameter("verbose", verbose)),
    solve = function(self, x, ...) {
      # extract data
      locked_in <- which(x$lb()[seq_len(x$number_of_actions())] > 0.5)
      locked_out <- which(x$ub()[seq_len(x$number_of_actions())] < 0.5)
      # generate solutions
      if (!inherits(x$data$objective, "MinimumSetObjective")) {
        runtime <- system.time({
          s <- rcpp_random_max_benefit_solution(
            x$data$action_costs(),
            locked_in, locked_out,
            x$data$objective$parameters$get("budget"),
            self$parameters$get("number_solutions"),
            as.logical(self$parameters$get("verbose")))
        })
      } else {
        runtime <- system.time({
          s <- rcpp_random_min_set_solution(
            x$data$action_costs(),
            x$data$pa_matrix(), x$data$epf_matrix(),
            as.list(x$data$targets$output()),
            locked_in, locked_out,
            self$parameters$get("number_solutions"),
            as.logical(self$parameters$get("verbose")))
        })
      }
      # format solution data
      lapply(seq_len(nrow(s)), function(i) {
        list(x = s[i, , drop = TRUE], objective = NA_real_,
             status = NA_character_, runtime = runtime)
      })
    }))
}
