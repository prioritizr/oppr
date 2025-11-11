#' @include internal.R ProjectModifier-class.R
NULL

#' @export
if (!methods::isClass("Weight")) methods::setOldClass("Weight")
NULL

#' Weight class
#'
#' @description
#' This class is used to represent targets for optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Weight-class
#'
#' @family classes
#'
#' @export
Weight <- R6::R6Class(
  "Weight",
  inherit = ProjectModifier,
  public = list(
    #' @description
    #' Output the targets.
    #' @return A `numeric` matrix.
    output = function() {
      # nocov start
      cli::cli_abort("No defined $output method.", .internal = TRUE)
      # nocov end
    }
  )
)
