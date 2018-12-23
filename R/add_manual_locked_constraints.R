#' @include internal.R pproto.R ProjectProblem-proto.R add_manual_targets.R
NULL

#' Add manually specified locked constraints
#'
#' Add constraints to a project prioritization \code{\link{problem}} to ensure
#' that solutions fund (or do not fund) specific actions. This function offers
#' more fine-grained control than the \code{\link{add_locked_in_constraints}}
#' and \code{\link{add_locked_out_constraints}} functions.
#'
#' @usage add_manual_locked_constraints(x, locked)
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param locked \code{data.frame} or \code{\link[tibble]{tibble}} object. See
#'   the Details section for more information.
#'
#' @details The argument to \code{locked} must contain the following fields
#'   (columns):
#'
#'   \describe{
#'
#'   \item{\code{"action"}}{\code{character} action name.}
#'
#'   \item{\code{"status"}}{\code{logical} values (i.e. \code{TRUE} or
#'     \code{FALSE}) indicating if actions should be funded or not.}
#'
#'   }
#'
#' @inherit add_locked_in_constraints return seealso
#'
#' @examples
#' #TODO
#'
#' @seealso \code{\link{constraints}}.
#'
#' @name add_manual_locked_constraints
#'
#' @exportMethod add_manual_locked_constraints
#'
#' @aliases add_manual_locked_constraints,ProjectProblem,data.frame-method add_manual_locked_constraints,ProjectProblem,tbl_df-method
#'
#' @export
methods::setGeneric("add_manual_locked_constraints",
                    signature = methods::signature("x", "locked"),
                    function(x, locked)
                      standardGeneric("add_manual_locked_constraints"))

#' @name add_manual_locked_constraints
#' @usage \S4method{add_manual_locked_constraints}{ProjectProblem,data.frame}(x, locked)
#' @rdname add_manual_locked_constraints
methods::setMethod("add_manual_locked_constraints",
  methods::signature("ProjectProblem", "data.frame"),
  function(x, locked) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ProjectProblem"),
                            inherits(locked, "data.frame"))
    # add constraints
    add_manual_locked_constraints(x, tibble::as.tibble(locked))
})

#' @name add_manual_locked_constraints
#' @usage \S4method{add_manual_locked_constraints}{ProjectProblem,tbl_df}(x, locked)
#' @rdname add_manual_locked_constraints
methods::setMethod("add_manual_locked_constraints",
  methods::signature("ProjectProblem", "tbl_df"),
  function(x, locked) {
    # define function to validate data
    validate_data <- function(locked) {
      assertthat::assert_that(inherits(x, "ProjectProblem"),
                              inherits(locked, "tbl_df"),
                              nrow(locked) > 0,
                              assertthat::has_name(locked, "action"),
                              inherits(locked$action, c("character", "factor")),
                              assertthat::noNA(locked$action),
                              all(locked$action %in%
                                  as.character(x$action_names())),
                              assertthat::has_name(locked, "status"),
                              is.logical(locked$status),
                              assertthat::noNA(locked$status))
    }
    # assert valid arguments
    validate_data(locked)
    # set attributes
    if (all(locked$status)) {
      class_name <- "LockedInConstraint"
      constraint_name <- "Locked in actions"
    } else if (all(!locked$status)) {
      class_name <- "LockedOutConstraint"
      constraint_name <- "Locked out actions"
    } else {
      class_name <- "LockedManualConstraint"
      constraint_name <- "Manually locked actions"
    }
    # define function to validate changes to data
    vfun <- function(x) !inherits(try(validate_data(x), silent = TRUE),
                                  "try-error")
    # define function to render data
    rfun <- function(x)
      utils::getFromNamespace("rHandsontableOutput", "rhandsontable")(x)
     # add constraints
     x$add_constraint(pproto(
      class_name,
      Constraint,
      name = constraint_name,
      data = list(action_names = x$action_names()),
      repr = function(self) {
        paste0(self$name, " [", nrow(self$parameters$get("Locked data")),
               " locked units]")
      },
      parameters = parameters(misc_parameter("Locked data", locked,
                                             vfun, rfun)),
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
          inherits(y, "ProjectProblem"))
        d <- self$parameters$get("Locked data")
        invisible(rcpp_apply_locked_constraints(x$ptr,
          match(d$action, self$data$action_names), as.integer(d$status)))
      }))
})
