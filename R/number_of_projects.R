#' @include internal.R
NULL

#' Number of projects
#'
#' Extract the number of projects in an object.
#'
#' @param x \code{\link{ProjectProblem-class}} or
#'   \code{\link{OptimizationProblem-class}} object.
#'
#' @return \code{integer} number of projects.
#'
#' @name number_of_projects
#'
#' @aliases number_of_projects,ProjectProblem-method number_of_projects,OptimizationProblem-method
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
#' # print number of projects
#' number_of_projects(p)
NULL

#' @name number_of_projects
#'
#' @rdname number_of_projects
#'
#' @exportMethod number_of_projects
#'
#' @usage number_of_projects(x)
#'
methods::setGeneric("number_of_projects",
  function(x) standardGeneric("number_of_projects"))

#' @name number_of_projects
#'
#' @rdname number_of_projects
#'
#' @usage \S4method{number_of_projects}{ProjectProblem}(x)
#'
methods::setMethod("number_of_projects", "ProjectProblem",
  function(x) x$number_of_projects())

#' @name number_of_projects
#'
#' @rdname number_of_projects
#'
#' @usage \S4method{number_of_projects}{OptimizationProblem}(x)
#'
methods::setMethod("number_of_projects", "OptimizationProblem",
  function(x) x$number_of_projects())
