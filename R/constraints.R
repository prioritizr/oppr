#' @include internal.R
NULL

#' Project prioritization problem constraints
#'
#' A constraint can be added to a project prioritization \code{\link{problem}}
#' to ensure that solutions exhibit a specific characteristic.
#'
#' @details
#'   The following constraints can be added to a project prioritization
#'   \code{\link{problem}}:
#'
#'   \describe{
#'
#'   \item{\code{\link{add_locked_in_constraints}}}{Add constraints to ensure
#'     that certain actions are prioritized for funding.}
#'
#'   \item{\code{\link{add_locked_out_constraints}}}{Add constraints to ensure
#'     that certain actions are not prioritized for funding.}
#'
#'  }
#'
#' @seealso \code{\link{decisions}}, \code{\link{objectives}},
#'  \code{\link{problem}}, \code{\link{solvers}}, \code{\link{targets}},
#'  \code{\link{weights}}.
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with maximum richness objective and $150 budget
#' p1 <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 150) %>%
#'      add_binary_decisions()
#'
#' # print problem
#' print(p1)
#'
#' # build another problem, and lock in the third action
#' p2 <- p1 %>%
#'       add_locked_in_constraints(c(3))
#'
#' # print problem
#' print(p2)
#'
#' # build another problem, and lock out the second action
#' p3 <- p1 %>%
#'       add_locked_out_constraints(c(2))
#'
#' # print problem
#' print(p3)
#'
#' \donttest{
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#'
#' # print the actions selected for funding in each of the solutions
#' print(s1[, sim_actions$name])
#' print(s2[, sim_actions$name])
#' print(s3[, sim_actions$name])
#' }
#' @name constraints
NULL
