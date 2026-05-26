#' @include internal.R ProjectProblem-class.R MultiObjProjectProblem-class.R
NULL

#' Action names
#'
#' Get the names of actions in an object.
#'
#' @param x [problem()] or [multi_problem()] object.
#'
#' @return A `character` vector.
#'
#' @name action_names
#'
#' @aliases action_names,ProjectProblem-method action_names,MultiObjProjectProblem-method
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#'
#' # print problem
#' print(p)
#'
#' # print action names
#' action_names(p)
NULL

#' @name action_names
#'
#' @rdname action_names
#'
#' @exportMethod action_names
#'
#' @usage action_names(x)
methods::setGeneric(
  "action_names",
  function(x) standardGeneric("action_names")
)

#' @name action_names
#'
#' @rdname action_names
#'
#' @usage \S4method{action_names}{ProjectProblem}(x)
methods::setMethod(
  "action_names", "ProjectProblem",
  function(x) x$action_names()
)

#' @name action_names
#'
#' @rdname action_names
#'
#' @usage \S4method{action_names}{MultiObjProjectProblem}(x)
methods::setMethod(
  "action_names", "MultiObjProjectProblem",
  function(x) x$action_names()
)
