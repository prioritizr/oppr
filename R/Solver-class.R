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

    #' @field has_pwlobj `logical` indicating if solver supports piece-wise
    #' linear components in an objective function.
    has_pwlobj = FALSE,

    #' @description
    #' Set start solution.
    #' @param x `numeric` vector.
    #' @return Invisible `TRUE`.
    set_start_solution = function(x) {
      self$set_data("start", x)
      invisible(TRUE)
    },

    #' @description
    #' Remove start solution.
    #' @param x `numeric` vector.
    #' @return Invisible `TRUE`.
    remove_start_solution = function(x) {
      self$set_data("start", new_waiver())
      invisible(TRUE)
    },

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
