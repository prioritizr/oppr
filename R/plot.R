#' @include internal.R ProjectProblem-proto.R solution_statistics.R
NULL

#' Plot a solution to a project prioritization problem
#'
#' Create a plot to visualize how well a solution to a project prioritization
#' problem will maintain biodiversity.
#'
#' @inheritParams solution_statistics
#'
#' @param n \code{integer} solution number to visualize.
#'   Since each row in the argument to \code{solutions} corresponds to a
#'   different solution, this argument should correspond to a row in
#'   the argument to \code{solutions}. Defaults to 1.
#'
#' @param symbol_hjust \code{numeric} horizontal adjustment parameter to
#'   manually align the asterisks and dashes in the plot. Defaults to
#'   \code{0.007}. Increasing this parameter will shift the symbols further
#'   right. Please note that this parameter may require some tweaking
#'   to produce visually appealing publication quality plots.
#'
#' @param return_data \code{logical} should the underlying data used to create
#'   the plot be returned instead of the plot? Defaults to \code{FALSE}.
#'
#' @param ... not used.
#'
#' @details The type of plot that this function creates depends on the
#'   problem objective. If the problem objective contains phylogenetic data,
#'   then this function plots a phylogenetic tree where each branch
#'   is colored according to its probability of persistence. Otherwise,
#'   if the problem does not contain phylogenetic data, then this function
#'   creates a bar plot where each bar corresponds to a different feature.
#'   The height of the bars indicate each feature's probability of
#'   persistence, and the width of the bars indicate each feature's weight.
#'
#'   Features that directly benefit from at least a single
#'   completely funded project with a non-zero cost are depicted with an
#'   asterisk symbol. Additionally, features that indirectly benefit from funded
#'   projects---because they are associated with partially funded projects that
#'   have non-zero costs and share actions with at least one funded
#'   project---are depicted with an open circle symbol.
#'
#' @return A \code{\link{ggplot}} object.
#'
#' @seealso This function is essentially a wrapper for
#'   \code{\link{plot_feature_persistence}} and
#'   \code{\link{plot_phylo_persistence}}, so refer to the documentation
#'   for these functions for more information.
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem without phylogenetic data
#' p1 <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'       add_max_richness_objective(budget = 400) %>%
#'       add_feature_weights("weight") %>%
#'       add_binary_decisions()
#'
#' \donttest{
#' # solve problem without phylogenetic data
#' s1 <- solve(p1)
#'
#' # visualize solution without phylogenetic data
#' plot(p1, s1)
#' }
#'
#' # build problem with phylogenetic data
#' p2 <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'       add_max_phylo_div_objective(budget = 400, sim_tree) %>%
#'       add_binary_decisions()
#'
#' \donttest{
#' # solve problem with phylogenetic data
#' s2 <- solve(p2)
#'
#' # visualize solution with phylogenetic data
#' plot(p2, s2)
#' }
#' @export
plot.ProjectProblem <- function(x, solution, n = 1, symbol_hjust = 0.007,
                                return_data = FALSE, ...) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          no_extra_arguments(...))
  assertthat::assert_that(!is.Waiver(x$objective),
    msg = "argument to x does not have a defined objective")
  # create plot
  if (inherits(x$objective, "MaximumPhyloDivObjective")) {
    g <- plot_phylo_persistence(x, solution, n = n, symbol_hjust = symbol_hjust,
                                return_data = return_data)
  } else {
    g <- plot_feature_persistence(x, solution, n = n,
                                  symbol_hjust = symbol_hjust,
                                  return_data = return_data)
  }
  g
}
