#' @include internal.R ProjectProblem-proto.R solution_statistics.R
NULL

#' Plot a project prioritization
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
#'   completely funded project with a non-zero cost are denoted with an
#'   asterisk symbol. Additionally, features that indirectly benefit from funded
#'   projects---because they are associated with partially funded projects that
#'   have non-zero costs and share actions with at least one funded
#'   project---are denoted with an open circle symbols.
#'
#' @return A \code{\link{ggplot}} object.
#'
#' @seealso \code{\link{plot_feature_persistence}},
#'   \code{\link{plot_phylo_persistence}}.
#'
#' @export
plot.ProjectProblem <- function(x, solution, n = 1, symbol_hjust = 0.007, ...) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          no_extra_arguments(...))
  assertthat::assert_that(!is.Waiver(x$objective),
    msg = "argument to x does not have a defined objective")
  # create plot
  if (inherits(x$objective, "MaximumPhyloDivObjective")) {
    g <- plot_phylo_persistence(x, solution, n = n, symbol_hjust = symbol_hjust)
  } else {
    g <- plot_feature_persistence(x, solution, n = n,
                                  symbol_hjust = symbol_hjust)
  }
  g
}
