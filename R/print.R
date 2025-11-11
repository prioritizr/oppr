#' @include internal.R
NULL

#' @method print ProjectProblem
#'
#' @export
print.ProjectProblem <- function(x, ...) x$print()

#' @method print ProjectModifier
#'
#' @export
print.ProjectModifier <- function(x, ...) x$print()

#' @method print OptimizationProblem
#'
#' @export
print.OptimizationProblem <- function(x, ...) x$print()
