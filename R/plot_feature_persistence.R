#' @include internal.R ProjectProblem-proto.R solution_statistics.R
NULL

#' Plot a bar plot to visualize a project prioritization
#'
#' Create a bar plot to visualize how likely features are to persist into
#' the future under a solution to a project prioritization problem.
#'
#' @inheritParams plot.ProjectProblem
#'
#' @details In this plot, each bar corresponds to a different feature.
#'   The length of each bar indicates the probability that a given feature
#'   will persist into the future, and the color of each bar indicates
#'   the weight for a given feature.
#'   Features that directly benefit from at least a single completely funded
#'   project with a non-zero cost are depicted with an asterisk symbol.
#'   Additionally, features that indirectly benefit from funded
#'   projects---because they are associated with partially funded
#'   projects that have non-zero costs and share actions with at least one
#'   completely funded project---are depicted with an open circle symbol.
#'
#' @return A \code{\link{ggplot}} object, or a
#'   \code{\link[tibble]{tbl_df}} object if \code{return_data} is
#'   \code{TRUE}.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load the ggplot2 R package to customize plots
#' library(ggplot2)
#'
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'       add_max_richness_objective(budget = 400) %>%
#'       add_feature_weights("weight") %>%
#'       add_binary_decisions() %>%
#'       add_heuristic_solver(n = 10)
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
#' # we can also obtain the raw plotting data using return_data=TRUE
#' plot_data <- plot(p, s, return_data = TRUE)
#' print(plot_data)
#' }
#' @export
plot_feature_persistence <- function(x, solution, n = 1, symbol_hjust = 0.007,
                                     return_data = FALSE) {
  # assertions
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
  # preliminary processing
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
  completely_funded_fts <- as.matrix(x$epf_matrix())[, x$feature_names(),
                                                    drop = FALSE] > 1e-15
  completely_funded_fts[!funded_projects, ] <- 0.0
  completely_funded_fts[zero_cost_projects, ] <- 0.0
  completely_funded_fts <- colSums(completely_funded_fts) > 1e-15
  completely_funded_fts <- x$feature_names()[completely_funded_fts]

  ## determine which species receive indirect based on sharing actions with
  ## a funded project
  partially_funded_fts <- as.matrix(x$epf_matrix())[, x$feature_names(),
                                                    drop = FALSE] > 1e-15
  partially_funded_fts[!partially_funded_projects, ] <- 0.0
  partially_funded_fts[zero_cost_projects, ] <- 0.0
  partially_funded_fts <- colSums(partially_funded_fts) > 1e-15
  partially_funded_fts <- setdiff(x$feature_names()[partially_funded_fts],
                                  completely_funded_fts)

  ## pre-compute probabilities that each branch will persist
  feature_probs <- rcpp_expected_persistences(
    x$pa_matrix(),
    x$epf_matrix()[, x$feature_names(), drop = FALSE],
    methods::as(diag(x$number_of_features()), "dgCMatrix"),
    methods::as(as.matrix(solution), "dgCMatrix"))[1, ]

  ## create plotting data
  d <- tibble::tibble(name = x$feature_names(),
                      prob = feature_probs,
                      weight = x$feature_weights(),
                      status = NA_character_)
  d$status[d$name %in% completely_funded_fts] <- "Funded"
  d$status[d$name %in% partially_funded_fts] <- "Partially Funded"
  d <- d[rev(seq_len(nrow(d))), ] # re-order data
  d$name <- factor(d$name, levels = unique(d$name))

  # Main processing
  ## prepare outputs
  if (!isTRUE(return_data)) {
    o <- ggplot2::ggplot(d, ggplot2::aes_string(x = "name", y = "prob",
                                           fill = "weight")) +
         ggplot2::geom_col() +
         ggplot2::geom_point(data = d[!is.na(d$status), , drop = FALSE],
                             ggplot2::aes_string(shape = "status"),
                             size = 3, color = "black",
                             position = ggplot2::position_nudge(
                               y =  symbol_hjust)) +
         ggplot2::scale_y_continuous(name = "Probability of persistence",
                                     limits = c(0, 1)) +
         ggplot2::xlab("") +
         ggplot2::scale_fill_gradientn(name = "Weight",
                                       colors = viridisLite::inferno(
                                         150, begin = 0, end = 0.9,
                                         direction = -1)) +
         ggplot2::scale_shape_manual(name = "Projects",
                                     values = c("Funded" = 8,
                                                "Partially Funded" = 1),
                                     na.translate = FALSE) +
         ggplot2::theme(legend.position = "right") +
         ggplot2::coord_flip()
  } else {
   o <- d
  }

  # Exports
  o
}
