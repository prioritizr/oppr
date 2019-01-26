#' @include internal.R
NULL

#' Solution statistics
#'
#' Calculate statistics describing a solution to a project prioritization
#' \code{\link{problem}}.
#'
#' @param x project prioritization \code{\link{problem}}.
#'
#' @param solution \code{\link[base]{data.frame}} or
#'   \code{\link[tibble]{tibble}} table containing the solutions. Here,
#'   rows correspond to different solutions and columns correspond to
#'   different actions. Each column in the argument to \code{solution} should
#'   be named according to a different action in \code{x}.
#'   Cell values indicate if an action is funded in a given solution or not,
#'   and should be either zero or one. Arguments to \code{solution} can
#'   contain additional columns, and they will be ignored.
#'
#' @return A \code{\link[tibble]{tibble}} table containing the following
#'   columns:
#'
#'   \describe{
#'
#'   \item{\code{"cost"}}{\code{numeric} cost of each solution.}
#'
#'   \item{\code{"obj"}}{\code{numeric} objective value for each solution.
#'     This is calculated using the objective function defined for the
#'     argument to \code{x}.}
#'
#'   \item{\code{x$project_names()}}{\code{numeric} column for each
#'     project indicating if it was completely funded (with a value of 1)
#'     or not (with a value of 0).}
#'
#'   \item{\code{x$feature_names()}}{\code{numeric} column for each
#'     feature indicating the probability that it will persist into
#'     the future given each solution.}
#'
#'   }
#'
#' @seealso \code{\link{objectives}}, \code{\link{replacement_costs}},
#'   \code{\link{project_cost_effectiveness}}.
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # print project data
#' print(sim_projects)
#'
#' # print action data
#' print(sim_features)
#'
#' # print feature data
#' print(sim_actions)
#'
#' # build problem
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 400) %>%
#'      add_feature_weights("weight") %>%
#'      add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
#' # create a table with some solutions
#' solutions <- data.frame(F1_action =       c(0, 1, 1),
#'                         F2_action =       c(0, 1, 0),
#'                         F3_action =       c(0, 1, 1),
#'                         F4_action =       c(0, 1, 0),
#'                         F5_action =       c(0, 1, 1),
#'                         baseline_action = c(1, 1, 1))
#'
#' # print the solutions
#' # the first solution only has the baseline action funded
#' # the second solution has every action funded
#' # the third solution has only some actions funded
#' print(solutions)
#'
#' # calculate statistics
#' solution_statistics(p, solutions)
#' @export
solution_statistics <- function(x, solution) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          inherits(solution, "data.frame"),
                          all(assertthat::has_name(solution, x$action_names())))
  assertthat::assert_that(!is.Waiver(x$objective),
    msg = "argument to x does not have an objective specified.")
  if (!inherits(solution, "tbl_df"))
    solution <- tibble::as_tibble(solution)
  # calculate cost and objective values
  out <- tibble::tibble(
    cost = rowSums(as.matrix(solution[, x$action_names()]) *
                   matrix(x$action_costs(), byrow = TRUE,
                          ncol = x$number_of_actions(),
                          nrow = nrow(solution))),
    obj = x$objective$evaluate(x, solution[, x$action_names()]))
  # add in columns indicating if each project is funded or not
  out <- tibble::as_tibble(cbind(out, stats::setNames(as.data.frame(
    rcpp_funded_projects(
      x$pa_matrix(),
      methods::as(as.matrix(solution[, x$action_names()]), "dgCMatrix"))),
    x$project_names())))
  # add in columns for feature persistences
  out <- tibble::as_tibble(cbind(out, stats::setNames(as.data.frame(
    rcpp_expected_persistences(
      x$pa_matrix(), x$epf_matrix(),
      methods::as(diag(x$number_of_features()), "dgCMatrix"),
      methods::as(as.matrix(solution[, x$action_names()]), "dgCMatrix"))),
      x$feature_names())))
  # return output
  out
}
