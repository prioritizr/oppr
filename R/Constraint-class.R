#' @include internal.R ProjectModifier-class.R
NULL

#' @export
if (!methods::isClass("Constraint")) methods::setOldClass("Constraint")
NULL

#' Constraint class
#'
#' @description
#' This class is used to represent the constraints used in optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Constraint-class
#'
#' @family classes
#'
#' @export
Constraint <- R6::R6Class(
  "Constraint",
  inherit = ProjectModifier
)
