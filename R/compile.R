#' @include internal.R ProjectProblem-proto.R OptimizationProblem-proto.R
NULL

#' Compile a problem
#'
#' Compile a project prioritization \code{\link{problem}} into a general
#' purpose format for optimization.
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param ... not used.
#'
#' @details This function might be useful for those interested in understanding
#'   how their project prioritization \code{\link{problem}} is expressed
#'   as a mathematical problem. However, if the problem just needs to
#'   be solved, then the \code{\link{solve}} function should be used instead.
#'
#' @return \code{\link{OptimizationProblem-class}} object.
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
#' # compile problem
#' o <- compile(p)
#'
#' # print compiled problem
#' print(o)
#' @export
compile <- function(x, ...) UseMethod("compile")

#' @rdname compile
#' @export
compile.ProjectProblem <- function(x, ...) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          no_extra_arguments(...))
  # list objectives that do not use targets
  not_target_based_obj <- c("MaximumPhyloDivObjective",
                            "MaximumRichnessObjective",
                            "MaximumPersistenceObjective")
  not_weight_based_obj <- c("MinimumSetObjective")
  # sanity checks
  if (!is.Waiver(x$targets) &&
      inherits(x$objective, not_target_based_obj))
    warning(paste("ignoring targets since the specified objective",
                  "function doesn't use targets"))
  if (!is.Waiver(x$weights) &&
      inherits(x$objective, not_weight_based_obj))
    warning(paste("ignoring weights since the specified objective",
                  "function doesn't use weights"))
  # replace waivers with defaults
  if (is.Waiver(x$objective))
    x <- add_default_objective(x)
  if (is.Waiver(x$targets) && !inherits(x$objective, not_target_based_obj))
    x <- add_default_targets(x)
  if (is.Waiver(x$decisions))
    x <- add_default_decisions(x)
  if (is.Waiver(x$solver))
    x <- add_default_solver(x)
  if (is.Waiver(x$weights) && !inherits(x$objective, not_weight_based_obj))
    x <- add_default_weights(x)
  op <- new_optimization_problem()
  # generate targets
  if (is.Waiver(x$targets)) {
    # if objective doesn't actually use targets, create a "fake" targets tibble
    # to initialize the problem data
    targets <- tibble::as_tibble(expand.grid(
      feature = seq_along(x$feature_names()),
      sense = "?",
      value = 0))
  } else {
    # generate "real" targets
    targets <- x$feature_targets()
  }
  # decompose and re-order phylogenetic data
  fp <- x$feature_phylogeny()
  bm <- branch_matrix(fp, FALSE)
  bo <- rcpp_branch_order(bm)
  # add raw data to optimization problem
  rcpp_add_raw_data(op$ptr, x$pa_matrix(),
                    x$epf_matrix()[, fp$tip.label, drop = FALSE],
                    bm[, bo, drop = FALSE],
                    fp$edge.length[bo], 1000)
  # add decision types to optimization problem
  x$decisions$calculate(x)
  x$decisions$apply(op)
  # add objective to optimization problem
  x$objective$calculate(x)
  x$objective$apply(op, x)
  # add weights to optimization problem
  if (!is.Waiver(x$weights) && !inherits(x$objective, not_weight_based_obj)) {
    x$weights$calculate(x)
    x$weights$apply(op, x)
  }
  # add constraints to optimization problem
  for (i in x$constraints$ids()) {
    x$constraints[[i]]$calculate(x)
    x$constraints[[i]]$apply(op, x)
  }
  # check that all actions have not been locked in
  if (all(op$lb()[seq_len(x$number_of_actions())] == 1))
    warning("all planning units are locked in.")
  # check that all actions have not been locked out
  if (all(op$ub()[seq_len(x$number_of_actions())] == 0))
    warning("all planning units are locked out.")
  # add data to optimization problem object
  op$data <- x
  # throw warning if range of values in object exceeds 1e8.
  # see: http://files.gurobi.com/Numerics.pdf
  r <- range(op$A()@x)
  if ((r[2] / r[1]) > 1e8)
    warning(paste("massive difference between minimum and maximum values in",
                  "the optimization problem, please double check that",
                  "solutions make sense and consider rescaling values."))
  # return optimization problem object
  op
}
