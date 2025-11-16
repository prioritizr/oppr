#' @include internal.R ProjectProblem-class.R MultiObjProjectProblem-class.R
NULL

#' Number of problems
#'
#' Extract the number of problems in an object.
#'
#' @inheritParams number_of_actions
#'
#' @return An `integer` value.
#'
#' @name number_of_problems
#'
#' @aliases number_of_problems,MultiObjProjectProblem-method
#'
#' @examples
#' # TODO
NULL

#' @name number_of_problems
#'
#' @rdname number_of_problems
#'
#' @exportMethod number_of_problems
#'
#' @usage number_of_problems(x)
methods::setGeneric(
  "number_of_problems",
  function(x) standardGeneric("number_of_problems")
)

#' @name number_of_problems
#'
#' @rdname number_of_problems
#'
#' @usage \S4method{number_of_problems}{MultiObjProjectProblem}(x)
methods::setMethod(
  "number_of_problems", "MultiObjProjectProblem",
  function(x) x$number_of_problems()
)
