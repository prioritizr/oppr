#' @include internal.R
NULL

#' Feature names
#'
#' Extract the names of the features in an object.
#'
#' @param x \code{\link{ProjectProblem-class}}.
#'
#' @return \code{character} feature names.
#'
#' @name feature_names
#'
#' @aliases feature_names,ProjectProblem-method
#'
#' @examples
#' #TODO
NULL

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @exportMethod feature_names
#'
#' @usage feature_names(x)
#'
methods::setGeneric("feature_names",
                    function(x) standardGeneric("feature_names"))

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{ProjectProblem}(x)
#'
methods::setMethod("feature_names", "ProjectProblem",
  function(x) x$feature_names())
