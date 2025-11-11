#' @include internal.R ProjectModifier-class.R
NULL

#' @export
if (!methods::isClass("Objective")) methods::setOldClass("Objective")
NULL

#' Objective class
#'
#' @description
#' This class is used to represent the objective function used in optimization.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Objective-class
#'
#' @family classes
#'
#' @export
Objective <- R6::R6Class(
  "Objective",
  inherit = ProjectModifier,
  public = list(
    #' @field has_targets `logical` value indicating if the objective uses
    #' targets.
    has_targets = NULL,

    #' @field has_weights `logical` value indicating if the objective uses
    #' weights.
    has_weights = NULL,

    #' @description
    #' Obtain the feature phylogeny.
    #' @return A [ape::phylo()] phylogenetic tree object.
    feature_phylogeny = function() {
      # nocov start
      cli::cli_abort("No phylogeny defined.")
      # nocov end
    },

    #' @description
    #' Obtain default feature weights.
    #' @return A `numeric` vector with the default feature weights.
    default_feature_weights = function() {
      # nocov start
      stop("No $default_feature_weights method defined.")
      # nocov end
    },

    #' @description
    #' Should default feature weights be replaced or multiplied by
    #' the new weights?
    #' @return A `logical` value.ks
    replace_feature_weights = function() {
      # nocov start
      stop("No $replace_feature_weights method defined.")
      # nocov end
    },

    #' @description
    #' Calculate the objective value for a solution.
    #' @param y [ProjectProblem-class] object.
    #' @param solution [tibble::tibble()] object with solution.
    #' @return A `numeric` value.
    evaluate = function(y, solution) {
      # nocov start
      stop("No $evaluate method defined.")
      # nocov end
    }
  )
)
