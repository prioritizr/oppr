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
#' #TODO
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
