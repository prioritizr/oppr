#' @include internal.R MultiObjApproach-class.R
NULL

#' Add a weighted goal achievement approach
#'
#' Add a weighted goal achievement approach for multi-objective
#' optimization to a project problem (Jones and Tamiz 2010).
#'
#' @inheritParams add_ref_point_approach
#'
#' @details
#' The weighted goal achievement approach for multi-objective
#' optimization involves
#' creating a new objective that is calculated based on multiple objectives.
#' In particular, the new objective uses weights to specify the relative
#' importance of each individual objective, and goals to specify
#' a threshold minimum level of performance for each objective
#' (conceptually similar to target thresholds used in conservation planning).
#' It then calculates the new objectives based on the weighted sum
#' of the percentage of each goal that is achieved for each objective.
#'
#' To describe this approach mathematically, we will define the
#' following terminology.
#' Let \eqn{O} denote the set of objectives (indexed by \eqn{o}).
#' For each objective, let \eqn{W_o}{W_o} denote the weight for each objective
#' \eqn{o \in O}{o in O}, \eqn{G_o}{G_o} denote the goal for each objective
#' \eqn{o \in O}{o in O}, and \eqn{V_o}{V_o} denote the objective value
#' for a candidate solution as measured based on each objective
#' \eqn{o \in O}{o in O}.
#' After defining these terms, the approach
#' is formulated with the following equation.
#'
#' \deqn{
#' \mathrm{Minimize} \space \sum_{o = 0}^{O} W_o \times \frac{V_o}{G_o}
#' }{
#' Minimize sum o^O W_o * (V_o / G_o)
#' }
#'
#' @inherit add_ref_point_approach return seealso
#'
#' @family approaches
#
#' @references
#' Jones D and Tamiz M (2010) _Goal Programming Variants_.
# 'In: Practical Goal Programming. International Series in Operations Research
#' and Management Science, volume 141. Springer, Boston, MA.
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
#'  add_wtd_goal_approach(weights = c(10, 11, 12), goals = c(3, 4, 5)) %>%
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
add_wtd_goal_approach <- function(x, weights, goals, verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "MultiObjProjectProblem"),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  if (is.numeric(weights) && !is.matrix(weights)) {
    weights <- matrix(weights, nrow = 1)
  }
  if (is.numeric(goals) && !is.matrix(goals)) {
    goals <- matrix(goals, nrow = 1)
  }
  assertthat::assert_that(
    is.matrix(weights),
    ncol(weights) == number_of_problems(x),
    nrow(weights) >= 1,
    assertthat::noNA(c(weights)),
    all(weights >= 0)
  )
  assertthat::assert_that(
    is.matrix(goals),
    ncol(goals) == number_of_problems(x),
    nrow(goals) >= 1,
    assertthat::noNA(c(goals)),
    all(goals > 0)
  )
  # add approach
  x$add_approach(
    R6::R6Class(
      "WeightedGoalApproach",
      inherit = MultiObjApproach,
      public = list(
        name = "weighted goal approach",
        data = list(weights = weights, goals = goals, verbose = verbose),
        run = function(x, solver) {
          ## initialization
          weights <- self$get_data("weights")
          goals <- self$get_data("goals")
          verbose <- self$get_data("verbose")
          sols <- vector(mode = "list", length = nrow(weights))
          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            pb <- cli::cli_progress_bar(
              "Generating solutions", total = nrow(weights)
            )
          }
          ## main processing
          for (i in seq_len(nrow(weights))) {
            ### copy optimization problem
            mo <- x$opt$copy()
            ### convert to formulation
            rcpp_convert_wtd_goal_method(
              mo$ptr, x$modelsense, x$obj,
              weights[i, ], goals[i, ]
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
