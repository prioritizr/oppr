#' @include internal.R ProjectProblem-class.R MultiObjProjectProblem-class.R
NULL

#' Number of problems
#'
#' Get the number of problems in an object.
#'
#' @inheritParams number_of_actions
#'
#' @return An `integer` value.
#'
#' @name number_of_problems
#'
#' @aliases number_of_problems,MultiObjProjectProblem-method
#'
#' @examples
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
#'         budget = 200, tree = sim_multi_tree[[1]]
#'       ) %>%
#'       add_binary_decisions(),
#'    obj2 =
#'      problem(
#'        sim_multi_projects[[2]], sim_multi_actions, sim_multi_features[[2]],
#'        "name", "success", "name", "cost", "name",
#'        baseline_project_name = "baseline_project_obj2"
#'      ) %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions(),
#'    obj3 =
#'      problem(
#'        sim_multi_projects[[3]], sim_multi_actions, sim_multi_features[[3]],
#'        "name", "success", "name", "cost", "name",
#'        baseline_project_name = "baseline_project_obj3"
#'      ) %>%
#'      add_max_wtd_sum_objective(budget = 200) %>%
#'      add_binary_decisions()
#'  )
#'
#' # print problem
#' print(p)
#'
#' # print number of problems
#' number_of_problems(p)
NULL

#' @name number_of_problems
#'
#' @rdname number_of_problems
#'
#' @exportMethod number_of_problems
#'
#' @usage number_of_problems(x)
methods::setGeneric(
  "number_of_problems",
  function(x) standardGeneric("number_of_problems")
)

#' @name number_of_problems
#'
#' @rdname number_of_problems
#'
#' @usage \S4method{number_of_problems}{MultiObjProjectProblem}(x)
methods::setMethod(
  "number_of_problems", "MultiObjProjectProblem",
  function(x) x$number_of_problems()
)
