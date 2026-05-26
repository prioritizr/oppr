#' @include internal.R waiver.R ProjectProblem-class.R waiver.R
NULL

#' @export
if (!methods::isClass("ProjectModifier")) methods::setOldClass("ProjectModifier")
NULL

#' Conservation problem modifier class
#'
#' This super-class is used to represent prototypes that in turn are used to
#' modify a [ProjectProblem-class] object. Specifically, the
#' [Constraint-class], [Decision-class],
#' [Objective-class], and [Target-class] prototypes
#' inherit from this class.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @family classes
#'
#' @name ProjectModifier-class
#'
#' @aliases ProjectModifier
#'
#' @export
ProjectModifier <- R6::R6Class(
  "ProjectModifier",
  public = list(
    #' @field name `character` value.
    name = character(0),

    #' @field data `list` containing data.
    data = list(),

    #' @field internal `list` containing internal computed values.
    internal = list(),

    #' @description
    #' Print information about the object.
    #' @return None.
    print = function() {
      message(self$repr())
    },

    #' @description
    #' Print information about the object.
    #' @return None.
    show = function() {
      self$print()
    },

    #' @description
    #' Generate a character representation of the object.
    #' @return A `character` value.
    repr = function() {
      self$name
    },

    #' @description
    #' Get values stored in the `data` field.
    #' @param x `character` name of data.
    #' @return An object. If the `data` field does not contain an object
    #' associated with the argument to `x`, then a [new_waiver()] object is
    #' returned.
    get_data = function(x) {
      if (!x %in% names(self$data)) {
        return(new_waiver())
      }
      return(self$data[[x]])
    },

    #' Set values stored in the `data` field. Note that this method will
    #' overwrite existing data.
    #' @param x `character` name of data.
    #' @param value Object to store.
    #' @return Invisible `TRUE`.
    set_data = function(x, value) {
      self$data[[x]] <- value
      invisible()
    },

    #' @description
    #' Get values stored in the `internal` field.
    #' @param x `character` name of data.
    #' @return An object. If the `internal` field does not contain an object
    #' associated with the argument to `x`, then a [new_waiver()] object is
    #' returned.
    get_internal = function(x) {
      if (!x %in% names(self$internal)) {
        return(new_waiver())
      }
      self$internal[[x]]
    },

    #' @description
    #' Set values stored in the `internal` field. Note that this method will
    #' overwrite existing data.
    #' @param x `character` name of data.
    #' @param value Object to store.
    #' @return An object. If the `internal` field does not contain an object
    #' associated with the argument to `x`, then a [new_waiver()] object is
    #' returned.
    set_internal = function(x, value) {
      self$internal[[x]] <- value
      invisible()
    },

    #' @description
    #' Perform computations that need to be completed before applying
    #' the object.
    #' @param x [new_optimization_problem()] object.
    #' @param y [problem()] object.
    #' @return Invisible `TRUE`.
    calculate = function(x, y) {
      invisible(TRUE)
    },

    #' @description
    #' Update an optimization problem formulation.
    #' @param x [new_optimization_problem()] object.
    #' @return Invisible `TRUE`.
    apply = function(x) {
      # nocov start
      stop("No defined $apply method.")
      # nocov end
    }
  )
)
