#' @include internal.R
NULL

#' Action names
#'
#' Extract the names of the actions in an object.
#'
#' @param x \code{\link{ProjectProblem-class}}.
#'
#' @return \code{character} action names.
#'
#' @name action_names
#'
#' @aliases action_names,ProjectProblem-method
#'
#' @examples
#' #TODO
NULL

#' @name action_names
#'
#' @rdname action_names
#'
#' @exportMethod action_names
#'
#' @usage action_names(x)
#'
methods::setGeneric("action_names",
                    function(x) standardGeneric("action_names"))

#' @name action_names
#'
#' @rdname action_names
#'
#' @usage \S4method{action_names}{ProjectProblem}(x)
#'
methods::setMethod("action_names", "ProjectProblem",
  function(x) x$action_names())
