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
#' #TODO
#'
#' @export
compile <- function(x, ...) UseMethod("compile")

#' @rdname compile
#' @export
compile.ProjectProblem <- function(x, ...) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          no_extra_arguments(...))
  # list objectives that do not use targets
  not_target_based_obj <- c("MaximumPhyloObjective",
                            "MaximumRichnessObjective",
                            "MaximumPersistenceObjective")
  # sanity checks
  if (inherits(x$objective, not_target_based_obj) &
      !is.Waiver(x$targets))
    warning(paste("ignoring targets since the specified objective",
                  "function doesn't use targets"))
  # replace waivers with defaults
  if (is.Waiver(x$objective))
    x <- add_default_objective(x)
  if (is.Waiver(x$targets) & !inherits(x$objective, not_target_based_obj))
    x <- add_default_targets(x)
  if (is.Waiver(x$decisions))
    x <- add_default_decisions(x)
  if (is.Waiver(x$solver))
    x <- add_default_solver(x)
  op <- new_optimization_problem()
  # generate targets
  if (is.Waiver(x$targets)) {
    # if objective doesn't actually use targets, create a "fake" targets tibble
    # to initialize the problem data
    targets <- tibble::as.tibble(expand.grid(
      feature = seq_along(x$feature_names()),
      zone = seq_along(x$zone_names()),
      sense = "?",
      value = 0))
  } else {
    # generate "real" targets
    targets <- x$feature_targets()
  }
  # add raw data to optimization problem
  fp <- x$feature_phylogeny()
  rcpp_add_raw_data(op$ptr, x$pa_matrix(), x$pf_matrix()[, fp$tip.label],
                    branch_matrix(fp), fp$edge.length, 1000)
  # add decision types to optimization problem
  x$decisions$calculate(x)
  x$decisions$apply(op)
  # add objective to optimization problem
  x$objective$calculate(x)
  x$objective$apply(op, x)
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
  op$data <- x$data
  # return optimization problem object
  op
}
