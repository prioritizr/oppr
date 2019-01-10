#' @include internal.R
NULL

#' Project names
#'
#' Extract the names of the projects in an object.
#'
#' @param x \code{\link{ProjectProblem-class}}.
#'
#' @return \code{character} project names.
#'
#' @name project_names
#'
#' @aliases project_names,ProjectProblem-method
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with default solver
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions() %>%
#'      add_default_solver()
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
#'
methods::setGeneric("project_names",
                    function(x) standardGeneric("project_names"))

#' @name project_names
#'
#' @rdname project_names
#'
#' @usage \S4method{project_names}{ProjectProblem}(x)
#'
methods::setMethod("project_names", "ProjectProblem",
  function(x) x$project_names())
