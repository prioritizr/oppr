#' @include internal.R ProjectModifier-class.R
NULL

#' @export
if (!methods::isClass("Decision")) methods::setOldClass("Decision")
NULL

#' Decision class
#'
#' @description
#' This class is used to represent the decision variables used in optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Decision-class
#'
#' @family classes
#'
#' @export
Decision <- R6::R6Class(
  "Decision",
  inherit = ProjectModifier
)
