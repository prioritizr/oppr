#' @include internal.R ProjectProblem-class.R MultiObjProjectProblem-class.R
NULL

#' Project names
#'
#' Get the names of the projects in an object.
#'
#' @inheritParams action_names
#'
#' @return A `character` vector or `list` of `character` vectors.
#'
#' @name project_names
#'
#' @aliases project_names,ProjectProblem-method project_names,MultiObjProjectProblem-method
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
#' # print project names
#' project_names(p)
NULL

#' @name project_names
#'
#' @rdname project_names
#'
#' @exportMethod project_names
#'
#' @usage project_names(x)
methods::setGeneric(
  "project_names",
  function(x) standardGeneric("project_names")
)

#' @name project_names
#'
#' @rdname project_names
#'
#' @usage \S4method{project_names}{ProjectProblem}(x)
methods::setMethod(
  "project_names", "ProjectProblem",
  function(x) x$project_names()
)

#' @name project_names
#'
#' @rdname project_names
#'
#' @usage \S4method{project_names}{MultiObjProjectProblem}(x)
methods::setMethod(
  "project_names", "MultiObjProjectProblem",
  function(x) x$project_names()
)
