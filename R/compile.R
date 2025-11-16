#' @include internal.R ProjectProblem-class.R OptimizationProblem-class.R
NULL

#' Compile a problem
#'
#' Compile a project prioritization [problem()] into a general
#' purpose format for optimization.
#'
#' @param x [ProjectProblem-class] object.
#'
#' @param n_approx `integer` number of points to use for piece-wise
#'   linear approximations of non-linear terms.
#'   Defaults to 100.
#'
#' @param ... not used.
#'
#' @details
#' This function might be useful for those interested in understanding
#' how their project prioritization [problem()] is expressed
#' as a mathematical problem. However, if the problem just needs to
#' be solved, then the [solve()] function should be used instead.
#'
#' @return An [OptimizationProblem-class] object.
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with maximum weighted sum objective, $200 budget, and
#' # binary decisions
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
#' # compile problem
#' o <- compile(p)
#'
#' # print compiled problem
#' print(o)
#' @export
compile <- function(x, ...) UseMethod("compile")

#' @rdname compile
#' @export
compile.ProjectProblem <- function(x, n_approx = 100, ...) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "ProjectProblem"),
    assertthat::is.count(n_approx),
    assertthat::noNA(n_approx),
    n_approx >= 3,
    no_extra_arguments(...)
  )
  # sanity checks
  if (isTRUE(x$defaults$targets) && isTRUE(x$objective$has_targets)) {
    stop("specified objective requires targets and no targets specified.")
  }
  if (!isTRUE(x$defaults$targets) && !isTRUE(x$objective$has_targets)) {
    warning(
      paste(
        "targets will be ignored because they are not used by the",
        "specified objective function."
      ),
      call. = FALSE, immediate. = TRUE
    )
  }
  if (!isTRUE(x$defaults$weights) && !isTRUE(x$objective$has_weights)) {
    warning(
      paste(
        "weights will be ignored because they are not used by the",
        "specified objective function."
      ),
      call. = FALSE, immediate. = TRUE
    )
  }
  # replace waivers with defaults
  if (isTRUE(x$defaults$objective)) {
    x <- add_default_objective(x)
  }
  if (isTRUE(x$defaults$targets) && isTRUE(x$objective$has_targets)) {
    x <- add_default_targets(x)
  }
  if (isTRUE(x$defaults$decisions)) {
    x <- add_default_decisions(x)
  }
  if (isTRUE(x$defaults$solver)) {
    x <- add_default_solver(x)
  }
  if (isTRUE(x$defaults$weights) && isTRUE(x$objective$has_weights)) {
    x <- add_default_weights(x)
  }
  op <- new_optimization_problem()
  # generate targets
  if (isTRUE(x$defaults$targets) || !isTRUE(x$objective$has_targets)) {
    # if objective doesn't actually use targets, create a "fake" targets tibble
    # to initialize the problem data
    targets <- tibble::tibble(
      feature = seq_along(x$feature_names()),
      sense = "?",
      value = 0
    )
  } else {
    # generate "real" targets
    targets <- x$feature_targets()
  }
  # decompose and re-order phylogenetic data
  fp <- x$feature_phylogeny()
  bm <- branch_matrix(fp, FALSE)
  bo <- rcpp_branch_order(bm)
  # add raw data to optimization problem
  rcpp_add_raw_data(
    op$ptr,
    x$pa_matrix(),
    x$eof_matrix()[, fp$tip.label, drop = FALSE],
    bm[, bo, drop = FALSE],
    fp$edge.length[bo],
    n_approx
  )
  # add decision types to optimization problem
  x$decisions$calculate(x)
  x$decisions$apply(op, x)
  # add objective to optimization problem
  x$objective$calculate(x)
  x$objective$apply(op, x)
  # add weights to optimization problem
  if (isTRUE(x$objective$has_weights)) {
    x$weights$calculate(x)
    x$weights$apply(op, x)
  }
  # add constraints to optimization problem
  for (i in seq_along(x$constraints)) {
    x$constraints[[i]]$calculate(x)
    x$constraints[[i]]$apply(op, x)
  }
  # check that all actions have not been locked in
  if (all(op$lb()[seq_len(x$number_of_actions())] == 1)) {
    warning(
      "all planning units are locked in.",
      call. = FALSE, immediate. = TRUE
    )
  }
  # check that all actions have not been locked out
  if (all(op$ub()[seq_len(x$number_of_actions())] == 0)) {
    warning(
      "all planning units are locked out.",
      call. = FALSE, immediate. = TRUE
    )
  }
  # add data to optimization problem object
  op$data <- x
  # throw warning if range of values in object exceeds 1e8.
  # see: http://files.gurobi.com/Numerics.pdf
  r <- range(op$A()@x)
  if ((r[2] / r[1]) > 1e8) {
    warning(
      paste(
        "massive difference between minimum and maximum values in",
        "the optimization problem, please double check that",
        "solutions make sense and consider rescaling values."
      ),
      call. = FALSE, immediate. = TRUE
    )
  }
  # return optimization problem object
  op
}
