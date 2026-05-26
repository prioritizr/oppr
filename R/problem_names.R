#' @include internal.R ProjectProblem-class.R MultiObjProjectProblem-class.R
NULL

#' Problem names
#'
#' Get the names of the problems in an object.
#'
#' @param x [multi_problem()] object.
#'
#' @return A `character` vector.
#'
#' @name problem_names
#'
#' @aliases problem_names,MultiObjProjectProblem-method
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
#' # print problem names
#' problem_names(p)
NULL

#' @name problem_names
#'
#' @rdname problem_names
#'
#' @exportMethod problem_names
#'
#' @usage problem_names(x)
methods::setGeneric(
  "problem_names",
  function(x) standardGeneric("problem_names")
)

#' @name problem_names
#'
#' @rdname problem_names
#'
#' @usage \S4method{problem_names}{MultiObjProjectProblem}(x)
methods::setMethod(
  "problem_names", "MultiObjProjectProblem",
  function(x) x$problem_names()
)
