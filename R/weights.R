#' @include internal.R pproto.R ProjectProblem-proto.R
NULL

#' Weights
#'
#' Weights are used to specify the relative importance for specific
#' features persisting into the future. Please note that only some
#' objectives require weights, and attempting to solve a problem that does not
#' require weights will throw a warning and the weights will be ignored.
#'
#' @details Currently, only one function can be used to specify weights:
#'
#'   \describe{
#'
#'   \item{[add_feature_weights()]}{
#'     Set feature weights for a project prioritization [problem()].}
#'
#'   }
#'
#' @seealso [constraints], [decisions],
#'  [objectives], [problem()],
#'  [solvers], [targets].
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with maximum richness objective, $300 budget, and
#' # feature weights
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_feature_weights("weight") %>%
#'      add_binary_decisions()
#'
#' \dontrun{
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#'
#' # plot solution
#' plot(p, s)
#' }
#' @name weights
NULL
