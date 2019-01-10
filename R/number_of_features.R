#' @include internal.R
NULL

#' Number of features
#'
#' Extract the number of features in an object.
#'
#' @param x \code{\link{ProjectProblem-class}} or
#'   \code{\link{OptimizationProblem-class}} object.
#'
#' @return \code{integer} number of features.
#'
#' @name number_of_features
#'
#' @aliases number_of_features,ProjectProblem-method number_of_features,OptimizationProblem-method
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
#' # print number of features
#' number_of_features(p)
NULL

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @exportMethod number_of_features
#'
#' @usage number_of_features(x)
#'
methods::setGeneric("number_of_features",
  function(x) standardGeneric("number_of_features"))

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @usage \S4method{number_of_features}{ProjectProblem}(x)
#'
methods::setMethod("number_of_features", "ProjectProblem",
  function(x) x$number_of_features())

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @usage \S4method{number_of_features}{OptimizationProblem}(x)
#'
methods::setMethod("number_of_features", "OptimizationProblem",
  function(x) x$number_of_features())
