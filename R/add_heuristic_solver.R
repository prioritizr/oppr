#' @include Solver-proto.R
NULL

#' Add a heuristic solver
#'
#' Specify that solutions should be generated using a backwards step-wise
#' heuristic algorithm (inspired by Cabeza \emph{et al.} 2004). Ideally,
#' solutions should be generated using exact algorithm solvers (e.g.
#' \code{\link{add_rsymphony_solver}} or \code{\link{add_gurobi_solver}}
#' because they can guaranteably identify optimal solutions (Rodrigues & Gaston
#' 2002).
#'
#' @inheritParams add_gurobi_solver
#'
#' @details The specific algorithm used to generate solutions depends on the
#'  the objective specified for the project prioritization
#'  \code{\link{problem}}.
#'
#'  For objectives which maximize benefit subject to budgetary constraints
#'  (e.g. \code{\link{add_max_richness_objective}}):
#'
#'  \itemize{
#'
#'  \item All actions are initially selected for funding (excepting actions
#'    which are locked out).
#'
#'  \item The next step is repeated until (i) the number of desired
#'    solutions is obtained and (ii) the total cost of the remaining
#'    actions that are selected for funding is within the budget.
#'
#'  \item Each of the remaining actions that are currently selected for
#'    funding (excepting actions which are locked in or actions which have zero
#'    cost) are evaluated according to how much the performance of the
#'    solution decreases when the action is defunded, relative to the cost
#'    of the action. This can be expressed mathematically as:
#'
#'    \deqn{B_l = \frac{V(L) - V(L - l)}{C_l}}{B_l = (V(L) - V(L - l)) / C_l}
#'
#'    Where \eqn{L} is the set of remaining actions currently
#'    selected for funding (indexed by \eqn{l}), \eqn{B_l} is the benefit
#'    associated with funding action \eqn{l}, \eqn{V(L)} is the objective
#'    value associated with the solution where all remaining actions are
#'    funded, \eqn{V(L - l)} is the objective value associated with the
#'    solution where all remaining actions except for the action \eqn{l} are
#'    funded, and \eqn{C_l} is the cost of action \eqn{l}.
#'
#'    The action with the smallest benefit (i.e. \code{B_l} value) is then
#'    defunded.
#'
#'  }
#'
#' For objectives which minimize cost subject to biodiversity constraints
#' (i.e. \code{\link{add_min_set_objective}}:
#'
#'  \itemize{
#'
#'  \item All actions are initially selected for funding (excepting actions
#'    which are locked out).
#'
#'  \item The next step is repeated until (i) the number of desired
#'    solutions is obtained or (ii) no action can be defunded
#'    without the probability of any feature expecting to persist into
#'    the future falling below its target probability of persistence..
#'
#'  \item Each of the remaining actions that are currently selected for
#'    funding (excepting actions which are locked in or actions which have zero
#'    cost) are evaluated according to how much the performance of the
#'    solution decreases when the action is defunded, relative to the cost
#'    of the action. This can be expressed mathematically as:
#'
#'    \deqn{B_l = \frac{\sum_{f}^{F} P_f(L) - T_f -
#'    \sum_{f}^{F} P_f(L - l) - T_f}{C_l}}{
#'    B_l = ((sum_f^F P_f(L) - T_f) - sum_f^F P_f(L - 1) - T_f)) / C_l}
#'
#'    Where \eqn{F} is the set of features (indexed by \eqn{f}),
#'    \eqn{T_f} is the target for feature \eqn{f}, \eqn{L} is the set of
#'    selected for funding (indexed by \eqn{l}), \eqn{C_l} is the cost of
#'     action \eqn{l}, \eqn{B_l} is the benefit
#'    associated with funding action \eqn{l}, \eqn{P(L)} is probability
#'    that each feature is expected to persist when the remaining actions
#'    (\eqn{L}) are funded, and \eqn{P(L - l)} is the probability that
#'    each feature is expected to persist when all the remaining actions and
#'    the action \code{l} are funded.
#'
#'    The action with the smallest benefit (i.e. \code{B_l} value) is then
#'    defunded.
#'
#'  }
#'
#' @inherit add_gurobi_solver seealso return
#'
#' @references
#' Rodrigues AS & Gaston KJ (2002) Optimisation in reserve selection
#' procedures---why not? \emph{Biological Conservation}, \strong{107},
#' 123--129.
#'
#' Cabeza M, Araujo MB, Wilson RJ, Thomas CD, Cowley MJ & Moilanen A (2004)
#' Combining probabilities of occurrence with spatial reserve design.
#' \emph{Journal of Applied Ecology}, \strong{41}, 252--262.
#'
#' @examples
#' #TODO
#'
#' @name add_heuristic_solver
NULL

#' @export
methods::setClass("HeuristicSolver", contains = "Solver")

#' @rdname add_heuristic_solver
#' @export
add_heuristic_solver <- function(x, number_solutions = 1, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          assertthat::is.count(number_solutions),
                          assertthat::noNA(number_solutions),
                          assertthat::is.flag(verbose),
                          assertthat::noNA(verbose))
  # add solver
  x$add_solver(pproto(
    "HeuristicSolver",
    Solver,
    name = "Heuristic",
    parameters = parameters(
      integer_parameter("number_solutions", number_solutions, lower_limit = 0L,
                        upper_limit = as.integer(.Machine$integer.max)),
      binary_parameter("verbose", verbose)),
    solve = function(self, x, ...) {
      # extract data
      locked_in <- which(x$lb()[seq_len(x$number_of_actions())] > 0.5)
      locked_out <- which(x$ub()[seq_len(x$number_of_actions())] < 0.5)
      # preliminary data calculations
      fp <- x$data$feature_phylogeny()
      bm <- branch_matrix(fp, FALSE)
      bo <- rcpp_branch_order(bm)
      w <- x$data$feature_weights()[fp$tip.label]
      if (!is.Waiver(x$data$targets)) {
        targets <- x$data$targets$output()$value
      } else {
        targets <- rep(0, length(fp$tip.label))
      }
      if (inherits(x$data$objective, "MinimumSetObjective")) {
        budget <- -1
      } else {
        budget <- x$data$objective$parameters$get("budget")
      }
      # generate solutions
      runtime <- system.time({
        s <- rcpp_heuristic_solution(
          x$data$action_costs(),
          x$data$pa_matrix(),
          x$data$epf_matrix(),
          bm[, bo, drop = FALSE], fp$edge.length[bo],
          targets, w, budget,
          locked_in, locked_out,
          self$parameters$get("number_solutions"),
          as.logical(self$parameters$get("verbose")),
          class(x$data$objective)[1])
      })
      # subset s if more solutions returned then desired
      if (nrow(s) > self$parameters$get("number_solutions"))
        s <- s[seq_len(self$parameters$get("number_solutions")), , drop = FALSE]
      # convert s to integers
      s <- round(s)
      # format solution data
      lapply(seq_len(nrow(s)), function(i) {
        list(x = s[i, , drop = TRUE], objective = NA_real_,
             status = NA_character_, runtime = runtime)
      })
    }))
}
