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
#' #TODO
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
