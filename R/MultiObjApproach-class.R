#' @include internal.R ProjectModifier-class.R
NULL

#' @export
if (!methods::isClass("MultiObjApproach")) methods::setOldClass("MultiObjApproach")
NULL

#' Multi-objective approach class
#'
#' @description
#' This class is used to represent approaches for multi-objective optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name MultiObjApproach-class
#'
#' @family classes
#'
#' @export
MultiObjApproach <- R6::R6Class(
  "MultiObjApproach",
  inherit = ProjectModifier,
  public = list(
    #' @description
    #' Perform computations that need to be completed before applying
    #' the object.
    #' @param x `list` containing a compiled multi-objective optimization
    #' problem (e.g., generated with internal function `multi_compile()`).
    #' @param y [multi_problem()] object.
    #' @return Invisible `TRUE`.
    calculate = function(x, y) {
      invisible(TRUE)
    },

    #' @description
    #' Solve a multi-objective optimization problem to generate a solution.
    #' @param x `list` containing a compiled multi-objective optimization
    #' problem (e.g., generated with internal function `multi_compile()`).
    #' @return `list` of solutions.
    run = function(x) {
      # nocov start
      cli::cli_abort("No defined $apply method.", .internal = TRUE)
      # nocov end
    }
  )
)
