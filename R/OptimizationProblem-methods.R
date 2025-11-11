#' @include internal.R OptimizationProblem-class.R
NULL

#' Convert `OptimizationProblem` to list
#'
#' @param x [OptimizationProblem-class] object.
#'
#' @param ... not used.
#'
#' @return [list()] object.
#'
#' @method as.list OptimizationProblem
#'
#' @rdname as.list
#'
#' @export
as.list.OptimizationProblem <- function(x, ...) {
  rcpp_optimization_problem_as_list(x$ptr)
}
