#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Specify the type of decisions
#'
#' Project prioritization problems involve making decisions about
#' how funding will be allocated to management actions.
#'
#' @details
#'   Please note that only one type of decision is currently supported:
#'
#'   \describe{
#'
#'   \item{\code{\link{add_binary_decisions}}}{
#'     This is the conventional type of decision where management actions are
#'     either prioritized for funding or not.}
#'
#'   }
#'
#' @seealso \code{\link{constraints}}, \code{\link{objectives}},
#'  \code{\link{problem}}, \code{\link{solvers}}, \code{\link{targets}},
#'  \code{\link{weights}}.
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with maximum richness objective, $200 budget, and
#' # binary decisions
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
#' \donttest{
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#'
#' # plot solution
#' plot(p, s)
#' }
#' @name decisions
NULL

add_default_decisions <- add_binary_decisions
