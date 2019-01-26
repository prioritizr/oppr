#' @include internal.R
NULL

#' Replacement cost
#'
#' Calculate the replacement cost for priority actions in a project
#' prioritization problem (Moilanen \emph{et al.} 2009). Actions associated
#' with larger replacement cost values are more irreplaceabe, and may
#' need to be implemented sooner than actions with lower replacement cost
#' values.
#'
#' @inheritParams solution_statistics
#'
#' @param n \code{integer} solution number to calculate replacement cost values.
#'   Since each row in the argument to \code{solutions} corresponds to a
#'   different solution, this argument should correspond to a row in
#'   the argument to \code{solutions}. Defaults to 1.
#'
#' @details Replacement cost values are calculated for each priority action
#'  specified in the solution. Missing (\code{NA}) values are assigned to
#'  actions which are not selected for funding in the specified solution.
#'  For a given action, its replacement cost is calculated by
#'  (i) calculating the objective value for the optimal solution to
#'  the argument to \code{x}, (ii) calculating the objective value for the
#'  optimal solution to the argument to \code{x} with the given action locked
#'  out, (iii) calculating the difference between the two objective
#'  values, (iv) the problem has an objective which aims to minimize
#'  the objective value (only \code{\link{add_min_set_objective}}, then
#'  the resulting value is multiplied by minus one so that larger values
#'  always indicate actions with greater irreplaceability. Please note this
#'  function can take a long time to complete
#'  for large problems since it involves re-solving the problem for every
#'  action selected for funding.
#'
#' @return A \code{\link[tibble]{tibble}} table containing the following
#'   columns:
#'
#'   \describe{
#'
#'   \item{\code{"action"}}{\code{character} name of each action.}
#'
#'   \item{\code{"cost"}}{\code{numeric} cost of each solution when each
#'     action is locked out.}
#'
#'   \item{\code{"obj"}}{\code{numeric} objective value of each solution when
#'     each action is locked out. This is calculated using the objective
#'     function defined for the argument to \code{x}.}
#'
#'   \item{\code{"rep_cost"}}{\code{numeric} replacement cost for each
#'     action. Greater values indicate greater irreplaceability. Missing
#'     (\code{NA}) values are assigned to actions which are not selected for
#'     funding in the specified solution, infinite (\code{Inf}) values are
#'     assigned to to actions which are required to meet feasibility
#'     constraints, and negative values mean that superior solutions than
#'     the specified solution exist.}
#'
#'   }
#'
#' @references
#' Moilanen A, Arponen A, Stokland JN & Cabeza M (2009) Assessing replacement
#' cost of conservation areas: how does habitat loss influence priorities?
#' \emph{Biological Conservation}, \strong{142}, 575--585.
#'
#' @seealso \code{\link{solution_statistics}},
#'   \code{\link{project_cost_effectiveness}}.
#'
#' @examples
#' \donttest{
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with maximum richness objective and $400 budget
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 400) %>%
#'      add_feature_weights("weight") %>%
#'      add_binary_decisions()
#'
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#'
#' # calculate replacement cost values
#' r <- replacement_costs(p, s)
#'
#' # print output
#' print(r)
#'
#' # plot histogram of replacement costs,
#' # with this objective, greater values indicate greater irreplaceability
#' hist(r$rep_cost, xlab = "Replacement cost", main = "")
#' }
#' @export
replacement_costs <- function(x, solution, n = 1) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "ProjectProblem"),
    inherits(solution, "data.frame"),
    all(assertthat::has_name(solution, x$action_names())),
    is.numeric(c(as.matrix(solution[, x$action_names()]))),
    assertthat::noNA(c(as.matrix(solution[, x$action_names()]))),
    assertthat::is.count(n),
    is.finite(n),
    isTRUE(n <= nrow(solution)))
  assertthat::assert_that(!is.Waiver(x$objective),
    msg = "argument to x does not have an objective specified.")
  if (!inherits(solution, "tbl_df"))
    solution <- tibble::as_tibble(solution)
  # over-write solver
  suppressWarnings({x <- add_default_solver(x, gap = 0, verbose = FALSE)})
  # calculate initial objective value
  obj <- try(solution_statistics(x, solution[n, x$action_names()])$obj,
             silent = TRUE)
  if (inherits(obj, "try-error"))
    stop("issue solving argument to x, please verify that it can be solved.")
  # find priority actions
  a <- which(c(as.matrix(solution[n, x$action_names()])) > 0.5)
  # calculate cost and objective values
  out <- lapply(a, function(i) {
    o <- try(solve(add_locked_out_constraints(x, i)), silent = TRUE)
    if (inherits(o, "try-error")) {
      o <- data.frame(cost = Inf, obj = Inf)
    } else {
      o <- o[, c("cost", "obj")]
    }
    o
  })
  out <- do.call(rbind, out)
  # prepare output
  out$name <- x$action_names()[a]
  out <- rbind(out, tibble::tibble(name = x$action_names()[-a],
                                   cost = NA_real_,
                                   obj = NA_real_))
  out <- out[match(x$action_names(), out$name), , drop = FALSE]
  out$rep_cost <- obj - out$obj
  # multiply by -1 if minimum set objective
  if (inherits(x$objective, "MinimumSetObjective"))
    out$rep_cost <- out$rep_cost * -1
  # return output
  out[, c("name", "cost", "obj", "rep_cost")]
}
