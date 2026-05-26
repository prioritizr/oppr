#' @include internal.R
NULL

#' Show
#'
#' Display information about an object.
#'
#' @param x Any object.
#'
#' @return None.
#'
#' @seealso [methods::show()].
#'
#' @name show
#'
#' @aliases show,ProjectProblem-method show,ProjectModifier-method show,OptimizationProblem-method show,MultiObjProjectProblem-method show,MultiObjApproach-method
NULL

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{ProjectModifier}(x)
methods::setMethod(
  "show", "ProjectModifier",
  function(object) object$show()
)

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{ProjectProblem}(x)
methods::setMethod(
  "show", "ProjectProblem",
  function(object) object$show()
)

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{OptimizationProblem}(x)
methods::setMethod(
  "show", "OptimizationProblem",
  function(object) object$show()
)

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{MultiObjProjectProblem}(x)
methods::setMethod(
  "show", "MultiObjProjectProblem",
  function(object) object$show()
)

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{MultiObjApproach}(x)
methods::setMethod(
  "show", "MultiObjApproach",
  function(object) object$show()
)
