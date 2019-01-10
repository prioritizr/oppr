#' @include internal.R
NULL

#' Number of actions
#'
#' Extract the number of actions in an object.
#'
#' @param x \code{\link{ProjectProblem-class}} or
#'   \code{\link{OptimizationProblem-class}} object.
#'
#' @return \code{integer} number of actions.
#'
#' @name number_of_actions
#'
#' @aliases number_of_actions,ProjectProblem-method number_of_actions,OptimizationProblem-method
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
#' # print number of actions
#' number_of_actions(p)
NULL

#' @name number_of_actions
#'
#' @rdname number_of_actions
#'
#' @exportMethod number_of_actions
#'
#' @usage number_of_actions(x)
#'
methods::setGeneric("number_of_actions",
  function(x) standardGeneric("number_of_actions"))

#' @name number_of_actions
#'
#' @rdname number_of_actions
#'
#' @usage \S4method{number_of_actions}{ProjectProblem}(x)
#'
methods::setMethod("number_of_actions", "ProjectProblem",
  function(x) x$number_of_actions())

#' @name number_of_actions
#'
#' @rdname number_of_actions
#'
#' @usage \S4method{number_of_actions}{OptimizationProblem}(x)
#'
methods::setMethod("number_of_actions", "OptimizationProblem",
  function(x) x$number_of_actions())
