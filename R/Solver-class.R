#' @include internal.R ProjectModifier-class.R

#' @export
if (!methods::isClass("Solver")) methods::setOldClass("Solver")
NULL

#' Solver class
#'
#' @description
#' This class is used to represent solvers for optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Solver-class
#'
#' @family classes
#'
#' @export
Solver <- R6::R6Class(
  "Solver",
  inherit = ProjectModifier,
  public = list(
    #' @description
    #' Solve an optimization problem.
    #' @param x [new_optimization_problem()] object.
    #' @param ... Additional arguments as needed.
    #' @return Invisible `TRUE`.
    solve = function(x, ...) {
      # nocov start
      stop("No defined $solve method.")
      # nocov end
    }
  )
)
