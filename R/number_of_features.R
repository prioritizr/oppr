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
#' #TODO
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
