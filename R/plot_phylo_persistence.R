#' @include internal.R ProjectProblem-proto.R solution_statistics.R
NULL

#' Plot a phylogram to visualize a project prioritization
#'
#' Create a plot showing a phylogenetic tree (i.e. a "phylogram") to visualize
#' the probability that phylogenetic branches are expected to persist
#' into the future under a solution to a project prioritization problem.
#'
#' @inheritParams plot.ProjectProblem
#'
#' @details This function requires the \pkg{ggtree} (Yu \emph{et al.} 2017).
#'   Since this package is distributed exclusively
#'   through \href{https://bioconductor.org}{Bioconductor}, and is not
#'   available on the
#'   \href{https://cran.r-project.org/}{Comprehensive R Archive Network},
#'   please execute the following command to install it:
#'   \code{source("https://bioconductor.org/biocLite.R");biocLite("ggtree")}.
#'   If the installation process fails, please consult the package's \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{online documentation}.
#'
#'   In this plot, each phylogenetic branch is colored according to probability
#'   that it is expected to persist into the future (see Faith 2008).
#'   Features that directly benefit from at least a single
#'   completely funded project with a non-zero cost are depicted with an
#'   asterisk symbol. Additionally, features that indirectly benefit from funded
#'   projects---because they are associated with partially funded projects that
#'   have non-zero costs and share actions with at least one completely funded
#'   project---are depicted with an open circle symbol.
#'
#' @references
#' Faith DP (2008) Threatened species and the potential loss of
#' phylogenetic diversity: conservation scenarios based on estimated extinction
#' probabilities and phylogenetic risk analysis. \emph{Conservation Biology},
#' \strong{22}: 1461--1470.
#'
#' Yu G, Smith DK, Zhu H, Guan Y, & Lam TTY (2017) ggtree: an
#' R package for visualization and annotation of phylogenetic trees with their
#' covariates and other associated data. \emph{Methods in Ecology and
#' Evolution}, \strong{8}: 28--36.
#'
#' @return A \code{\link[ggtree]{ggtree}} object, or a
#'   \code{\link[tidytree]{treedata}} object if \code{return_data} is
#'   \code{TRUE}.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load the ggplot2 R package to customize plots
#' library(ggplot2)
#'
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_phylo_div_objective(budget = 400, sim_tree) %>%
#'      add_binary_decisions() %>%
#'      add_heuristic_solver(number_solutions = 10)
#'
#' \donttest{
#' # solve problem
#' s <- solve(p)
#'
#' # plot the first solution
#' plot(p, s)
#'
#' # plot the second solution
#' plot(p, s, n = 2)
#'
#' # since this function returns a ggplot2 plot object, we can customize the
#' # appearance of the plot using standard ggplot2 commands!
#' # for example, we can add a title
#' plot(p, s) + ggtitle("solution")
#'
#' # we could also also set the minimum and maximum values in the color ramp to
#' # correspond to those in the data, rather than being capped at 0 and 1
#' plot(p, s) +
#' scale_color_gradientn(name = "Probability of\npersistence",
#'                       colors = viridisLite::inferno(150, begin = 0,
#'                                                     end = 0.9,
#'                                                     direction = -1)) +
#' ggtitle("solution")
#'
#' # we could also change the color ramp
#' plot(p, s) +
#' scale_color_gradient(name = "Probability of\npersistence",
#'                      low = "red", high = "black") +
#' ggtitle("solution")
#'
#' # we could even hide the legend if desired
#' plot(p, s) +
#' scale_color_gradient(name = "Probability of\npersistence",
#'                      low = "red", high = "black") +
#' theme(legend.position = "hide") +
#' ggtitle("solution")
#'
#' # we can also obtain the raw plotting data using return_data=TRUE
#' plot_data <- plot(p, s, return_data = TRUE)
#' print(plot_data)
#' }
#' @export
plot_phylo_persistence <- function(x, solution, n = 1, symbol_hjust = 0.007,
                                   return_data = FALSE) {
  # assertions
  ## assert that ggtree R package is installed
  assertthat::assert_that(requireNamespace("ggtree", quietly = TRUE),
                          msg = "ggtree R package not installed.")
  ## coerce solution to tibble if just a regular data.frame
  if (inherits(solution, "data.frame") && !inherits(solution, "tbl_df"))
    solution <- tibble::as_tibble(solution)
  ## assert that parameters are valid
  assertthat::assert_that(
    inherits(x, "ProjectProblem"),
    inherits(solution, "tbl_df"),
    ncol(solution) > 0, nrow(solution) > 0,
    all(assertthat::has_name(solution, x$action_names())),
    is.numeric(as.matrix(solution[, x$action_names()])),
    assertthat::noNA(c(as.matrix(solution[, x$action_names()]))),
    max(as.matrix(solution[, x$action_names()])) <= 1,
    min(as.matrix(solution[, x$action_names()])) >= 0,
    assertthat::is.count(n),
    is.finite(n),
    isTRUE(n <= nrow(solution)),
    assertthat::is.number(symbol_hjust),
    is.finite(symbol_hjust),
    assertthat::is.flag(return_data),
    assertthat::noNA(return_data))
  assertthat::assert_that(!is.Waiver(x$objective),
    msg = "argument to x does not have a defined objective")
  # preliminary data processing
  ## extract tree
  tree <- x$feature_phylogeny()

  ## subset solution and reorder columns
  solution <- solution[n, x$action_names(), drop = FALSE]

  ## determine which projects are funded based on the funded actions
  ## and omit the baseline project
  prj <- as.matrix(x$pa_matrix())
  funding_matrix <- matrix(TRUE, ncol = x$number_of_actions(),
                           nrow = x$number_of_projects())
  pos <- which(prj > 0.5, arr.ind = TRUE)
  funding_matrix[pos] <- as.matrix(solution)[1, pos[, 2]]
  funded_projects <- rowSums(funding_matrix) == ncol(prj)
  partially_funded_projects <- (rowSums(funding_matrix) > 0) &
                               (rowSums(!funding_matrix ) < rowSums(prj)) &
                               (rowSums(funding_matrix) != ncol(prj))

  ## determine baseline project(s)
  zero_cost_projects <- rowSums(as.matrix(x$pa_matrix()) *
                                matrix(x$action_costs(), byrow = TRUE,
                                       ncol = x$number_of_actions(),
                                       nrow = x$number_of_projects()))
  zero_cost_projects <- zero_cost_projects < 1e-15

  ## determine which features receive funding based on their project being
  ## funded
  completely_funded_fts <- as.matrix(x$epf_matrix())[, tree$tip.label,
                                                     drop = FALSE] > 1e-15
  completely_funded_fts[!funded_projects, ] <- 0.0
  completely_funded_fts[zero_cost_projects, ] <- 0.0
  completely_funded_fts <- colSums(completely_funded_fts) > 1e-15
  completely_funded_fts <- tree$tip.label[completely_funded_fts]

  ## determine which species receive indirect based on sharing actions with
  ## a funded project
  partially_funded_fts <- as.matrix(x$epf_matrix())[, tree$tip.label,
                                                     drop = FALSE] > 1e-15
  partially_funded_fts[!partially_funded_projects, ] <- 0.0
  partially_funded_fts[zero_cost_projects, ] <- 0.0
  partially_funded_fts <- colSums(partially_funded_fts) > 1e-15
  partially_funded_fts <- setdiff(tree$tip.label[partially_funded_fts],
                                  completely_funded_fts)

  ## pre-compute probabilities that each branch will persist
  branch_probs <- rcpp_expected_persistences(
    x$pa_matrix(), x$epf_matrix()[, tree$tip.label, drop = FALSE],
    branch_matrix(tree), methods::as(as.matrix(solution), "dgCMatrix"))

  # Main processing
  ## format tree data for plotting
  tree2 <- suppressMessages(suppressWarnings(tidytree::as_tibble(tree)))
  tree2$status <- NA_character_
  tree2$status[tree2$label %in% completely_funded_fts] <- "Funded"
  tree2$status[tree2$label %in% partially_funded_fts] <- "Partially Funded"
  tree2$prob <- c(branch_probs)[match(
    paste0(tree2$parent, "_", tree2$node),
    paste0(tree$edge[, 1], "_", tree$edge[, 2]))]
  tree2$label <- paste0("   ", tree2$label)
  tree2 <- suppressMessages(suppressWarnings(tidytree::as.treedata(tree2)))

  ## calculate padding for points
  point_padding <- max(rowSums(as.matrix(branch_matrix(tree)) *
                               matrix(tree$edge.length, ncol = nrow(tree$edge),
                                      nrow = length(tree$tip.label)))) *
                   symbol_hjust

  ## prepare outputs
  if (!isTRUE(return_data)) {
    o <- ggtree::ggtree(tree2, ggplot2::aes_string(color = "prob"),
                        size = 1.1) +
         ggtree::geom_tippoint(
           ggplot2::aes_string(x = "x + point_padding",
                               subset = "!is.na(status)",
                               shape = "status"), color = "black") +
         ggtree::geom_tiplab(color = "black", size = 2.5) +
         ggplot2::scale_color_gradientn(name = "Probability of\npersistence",
                                        colors = viridisLite::inferno(
                                          150, begin = 0, end = 0.9,
                                          direction = -1),
                                        limits = c(0, 1)) +
         ggplot2::scale_shape_manual(name = "Projects",
                                     values = c("Funded" = 8,
                                                "Partially Funded" = 1),
                                     na.translate = FALSE) +
         ggplot2::theme(legend.position = "right")
  } else {
    o <- tree2
  }
  # Exports
  # return plot
  o
}
