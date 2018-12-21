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
#' #TODO
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
