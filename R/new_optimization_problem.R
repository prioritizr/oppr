#' @include internal.R OptimizationProblem-class.R
NULL

#' Optimization problem
#'
#' Generate a new empty [OptimizationProblem-class] object.
#'
#' @return An [OptimizationProblem-class] object.
#'
#' @examples
#' # create empty OptimizationProblem object
#' x <- new_optimization_problem()
#'
#' # print new object
#' print(x)
#' @export
new_optimization_problem <- function() {
  OptimizationProblem$new(ptr = rcpp_new_optimization_problem())
}
