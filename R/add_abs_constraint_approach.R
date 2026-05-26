#' @include internal.R MultiObjApproach-class.R
NULL

#' Add an absolute constraint approach
#'
#' Add an constraint approach for multi-objective optimization to a project
#' problem based on the required objective values.
#'
#' @inheritParams add_ref_point_approach
#'
#' @param goals `numeric` vector containing values that denote the
#' represent a threshold minimum level of achievement for each objective.
#' If no goal is required for a particular objective, then
#' a missing `NA` value can be specified.
#' To generate multiple solutions based on different values,
#' `goals` can be a `numeric` matrix where
#' each row corresponds to a different solution and each columns
#' corresponds to a different objective.
#'
#' @details
#' Constraint-based approaches for multi-objective optimization involves
#' adding constraints to a problem formulation to ensure that solutions
#' achieve a particular level of performance each objective,
#' whilst maximizing performance according to a primary objective.
#' In particular, each objective is associated with a goal that specifies
#' a threshold minimum level of performance (conceptually similar to a target
#' in the minimum set formulation).
#' Below we provide the mathematical details for this approach.
#'
#' To describe this approach mathematically, we will define the
#' following terminology.
#' Let \eqn{O} denote the set of objectives (indexed by \eqn{o}).
#' For each objective, \eqn{G_o}{G_o} denote the goal for each objective
#' \eqn{o \in O}{o in O}, and \eqn{V_o}{V_o} denote the objective value
#' for a candidate solution as measured based on each objective
#' \eqn{o \in O}{o in O}. Also, let \eqn{V_1} denote the
#' objective value for the first objective.
#' Although we assume that all objectives here should be maximized
#' (for brevity), this approach is compatible with objectives that should
#' also be minimized.
#' After defining these terms, the approach
#' is formulated with the following equation.
#'
#' \deqn{
#' \mathrm{Maximize} \space V_1 \\
#' \mathrm{Subject \space to} \space \\
#' V_o \geq G_o \space \forall o \in O
#' }{
#' Maximize V_1, Subject to V_o >= G_o, for all o in O
#' }
#'
#' @inherit add_ref_point_approach return seealso
#'
#' @family approaches
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_multi_projects)
#' data(sim_multi_features)
#' data(sim_multi_actions)
#' data(sim_multi_tree)
#'
#' # build problem
#' p <-
#'   multi_problem(
#'     obj1 =
#'       problem(
#'         sim_multi_projects[[1]], sim_multi_actions, sim_multi_features[[1]],
#'         "name", "success", "name", "cost", "name",
#'         baseline_project_name = "baseline_project_obj1"
#'       ) %>%
#'       add_max_phylo_div_objective(
#'         budget = 1000, tree = sim_multi_tree[[1]]
#'       ) %>%
#'       add_binary_decisions(),
#'    obj2 =
#'      problem(
#'        sim_multi_projects[[2]], sim_multi_actions, sim_multi_features[[2]],
#'        "name", "success", "name", "cost", "name",
#'        baseline_project_name = "baseline_project_obj2"
#'      ) %>%
#'      add_max_richness_objective(budget = 1000) %>%
#'      add_binary_decisions(),
#'    obj3 =
#'      problem(
#'        sim_multi_projects[[3]], sim_multi_actions, sim_multi_features[[3]],
#'        "name", "success", "name", "cost", "name",
#'        baseline_project_name = "baseline_project_obj3"
#'      ) %>%
#'      add_max_wtd_sum_objective(budget = 1000) %>%
#'      add_binary_decisions()
#'  ) %>%
#'  add_abs_constraint_approach(goals = c(NA, 0.01, 0.01)) %>%
#'  add_default_solver()
#'
#' # print problem
#' print(p)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#' @export
add_abs_constraint_approach <- function(x, goals, verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "MultiObjProjectProblem"),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  if (is.numeric(goals) && !is.matrix(goals)) {
    goals <- matrix(goals, nrow = 1)
  }
  assertthat::assert_that(
    is.matrix(goals),
    ncol(goals) == number_of_problems(x),
    nrow(goals) >= 1,
    is.numeric(goals)
  )
  # add approach
  x$add_approach(
    R6::R6Class(
      "AbsConstraintApproach",
      inherit = MultiObjApproach,
      public = list(
        name = "absolute constraint approach",
        data = list(goals = goals, verbose = verbose),
        run = function(x, solver) {
          ## initialization
          goals <- self$get_data("goals")
          verbose <- self$get_data("verbose")
          sols <- vector(mode = "list", length = nrow(goals))
          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            pb <- cli::cli_progress_bar(
              "Generating solutions", total = nrow(goals)
            )
          }
          ## main processing
          for (i in seq_len(nrow(goals))) {
            ### copy optimization problem
            mo <- x$opt$copy()
            ### convert to absolute constraint formulation
            rcpp_convert_abs_constraint_approach(
              mo$ptr, x$modelsense, x$obj, goals[i, ]
            )
            ### solve problem
            sols[[i]] <- solver$solve(mo)
            ## if needed, update progress bar
            if (isTRUE(verbose)) {
              cli::cli_progress_update(id = pb)
            }
          }
          ## if needed, clean up progress bar
          if (isTRUE(verbose)) {
            cli::cli_progress_done(id = pb)
          }
          ## prepare solutions for output
          n_sol <- sum(lengths(sols))
          out <- vector(mode = "list", length = n_sol)
          k <- 0
          for (i in seq_along(sols)) {
            for (j in seq_along(sols[[i]])) {
              k <- k + 1
              out[[k]] <- sols[[i]][[j]]
            }
          }
          ## return solutions
          out
        }
      )
    )$new()
  )
}
