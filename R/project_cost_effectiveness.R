#' @include internal.R
NULL

#' Project cost effectiveness
#'
#' Calculate the individual cost-effectiveness of each conservation project
#' in a project prioritization \code{\link{problem}}
#' (Joseph, Maloney & Possingham 2009).
#'
#' @param x project prioritization \code{\link{problem}}.
#'
#' @details Note that project cost-effectiveness cannot be calculated for
#'   problems with minimum set objectives because the objective function
#'   for these problems is to minimize cost and not maximize some measure
#'   of biodiversity persistence.
#'
#' @return A \code{\link[tibble]{tibble}} table containing the following
#'   columns:
#'
#'   \describe{
#'
#'   \item{\code{"project"}}{\code{character} name of each project}
#'
#'   \item{\code{"cost"}}{\code{numeric} cost of each project.}
#'
#'   \item{\code{"benefit"}}{\code{numeric} benefit for each project. For a
#'     given project, this is calculated as the difference between (i) the
#'     objective value for a solution containing all of the management actions
#'     associated with the project and all zero cost actions, and (ii) the
#'     objective value for a solution containing the baseline project.}
#'
#'   \item{\code{"ce"}}{\code{numeric} cost-effectiveness of each project.
#'     For a given project, this is calculated as the difference between the
#'     the benefit for the project and the benefit for the baseline project,
#'     divided by the cost of the project. Note that the baseline
#'     project will have a \code{NaN} value because it has a zero cost.}
#'
#'  \item{\code{"rank"}}{\code{numeric} rank for each project according to
#'    is cost-effectiveness value. The project with a rank of one is the
#'    most cost-effective project. Ties are accommodated using averages.}
#'
#'   }
#'
#' @references
#' Joseph LN, Maloney RF & Possingham HP (2009) Optimal allocation of
#' resources among threatened species: A project prioritization protocol.
#' \emph{Conservation Biology}, \strong{23}, 328--338.
#'
#' @seealso \code{\link{solution_statistics}}, \code{\link{replacement_costs}}.
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
#' # calculate cost-effectiveness of each project
#' pce <- project_cost_effectiveness(p)
#'
#' # print project costs, benefits, and cost-effectiveness values
#' print(pce)
#'
#' # plot histogram of cost-effectiveness values
#' hist(pce$ce, xlab = "Cost effectiveness", main = "")
#' @export
project_cost_effectiveness <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"))
  assertthat::assert_that(!is.Waiver(x$objective),
    msg = "argument to x does not have an objective specified.")
  assertthat::assert_that(!inherits(x$objective, "MinimumSetObjective"),
    msg = paste0("project cost effectiveness values cannot be (meaningfully) ",
                 "computed for minimum set problems."))
  # generate baseline- project solution
  bpm <- matrix(x$action_costs() == 0, nrow = 1,
                dimnames = list(NULL, x$action_names()))
  bp_obj <- x$objective$evaluate(x, tibble::as_tibble(bpm))
  # generate solutions for other projects
  bpm <- bpm[rep(1, x$number_of_projects()), , drop = FALSE]
  pp <- methods::as(x$pa_matrix(), "lgCMatrix") |
        methods::as(bpm, "lgCMatrix")
  pp <- tibble::as_tibble(round(as.matrix(pp)))
  # evaluate solutions
  pp_obj <- x$objective$evaluate(x, pp)
  pp_costs <- x$project_costs()
  pp_ce <- (pp_obj - bp_obj) / pp_costs
  # return result
  tibble::tibble(project = x$project_names(),
                 cost = unname(pp_costs),
                 obj = pp_obj,
                 benefit = pp_obj - bp_obj,
                 ce = unname(pp_ce),
                 rank = unname(rank(-pp_ce)))
}
