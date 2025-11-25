#' @include MultiObjApproach-class.R
NULL

#' Multi-objective optimization approaches
#'
#' Approaches specify methods for generating solutions for
#' multi-objective optimization problems.
#'
#' @details
#' The following approaches can be used to generate solutions for a
#' multi-objective project prioritization problem.
#'
#' \describe{
#'
#' \item{[add_abs_constraint_approach()]}{
#' Add an approach to generate solutions based on constraints
#' that specify the required objectives values.
#' }
#'
#' \item{[add_ref_point_approach()]}{
#' Add an approach to generate solutions with the reference point method.
#' }
#'
#' }
#'
#' @name approaches
#'
#' @family overviews
#'
#' @examples
#' \dontrun{
#' # load data
#' data(sim_multi_projects)
#' data(sim_multi_features)
#' data(sim_multi_actions)
#' data(sim_multi_tree)
#'
#' # build problem
#' p1 <-
#'   multi_problem(
#'     obj1 =
#'       problem(
#'         sim_multi_projects[[1]], sim_multi_actions, sim_multi_features[[1]],
#'         "name", "success", "name", "cost", "name",
#'         baseline_project_name = "baseline_project_obj1"
#'       ) %>%
#'       add_max_phylo_div_objective(
#'        budget = 1000, tree = sim_multi_tree[[1]]
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
#'  add_default_solver()
#'
#' # build another problem, with the reference point method
#' p2 <-
#'   p1 %>%
#'   add_ref_point_approach(
#'     weights = c(1, 0.5, 0.1),
#'     goals = c(1, 3, 0.2),
#'     method = "sum"
#'   )
#'
#' # build another problem, with the absolute constraint method
#' p3 <-
#'   p1 %>%
#'   add_abs_constraint_approach(
#'     goals = c(NA, 0.01, 0.01)
#'   )
#'
#'
#' # generate solutions using each approach
#' s <- rbind(solve(p2), solve(p3))
#' s$approach <- c("ref point", "abs epsilon")
#'
#' # print solutions
#' print(as.data.frame(s))
#' }
NULL
