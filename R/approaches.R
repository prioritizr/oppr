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
#' \item{[add_wtd_goal_approach()]}{
#' Add an approach to generate solutions with the weighted goal method
#' (Jones and Tamiz 2010).
#' }
#'
#' \item{[add_ref_point_approach()]}{
#' Add an approach to generate solutions with the reference point method
#' (Vanderpooten 1990)
#' }
#'
#' }
#'
#' @name approaches
#'
#' @family overviews
#'
#' @references
#' Jones D and Tamiz M (2010) _Goal Programming Variants_.
# 'In: Practical Goal Programming. International Series in Operations Research
#' and Management Science, volume 141. Springer, Boston, MA.
#'
#' Vanderpooten D (1990) _Multiobjective programming: Basic concepts and_
#' _approaches_. In: Stochastic Versus Fuzzy Approaches to Multiobjective
#' Mathematical Programming Under Uncertainty. Springer, Berlin.
#'
#' @examplesIf oppr::run_example()
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
#' # build another problem, with the absolute constraint method
#' p2 <-
#'   p1 %>%
#'   add_abs_constraint_approach(
#'     goals = c(NA, 0.01, 0.01)
#'   )
#'
#' # build another problem, with the weighted goal method
#' p3 <-
#'   p1 %>%
#'   add_wtd_goal_approach(
#'     weights = c(1, 0.5, 0.1),
#'     goals = c(1, 3, 0.2)
#'   )
#'
#' # build another problem, with the reference point method
#' p4 <-
#'   p1 %>%
#'   add_ref_point_approach(
#'     weights = c(1, 0.5, 0.1),
#'     goals = c(1, 3, 0.2)
#'   )
#'
#' # generate solutions using each approach
#' s <- rbind(solve(p2), solve(p3), solve(p4))
#' s$approach <- c("abs epsilon", "wtd goal", "ref point")
#'
#' # print solutions
#' print(as.data.frame(s))
NULL
