#' @include internal.R Constraint-proto.R
NULL

#' Add locked in constraints
#'
#' Add constraints to a project prioritization \code{\link{problem}} to ensure
#' that specific actions are funded in the solution. For example, it may be
#' desirable to lock in actions for conserving culturally or taxonomically
#' important species. If specific actions should be locked out of a solution,
#' use \code{\link{add_locked_out_constraints}}.
#'
#' @usage add_locked_in_constraints(x, locked_in)
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param locked_in Object that determines which planning units that should be
#'   locked in. See the Details section for more information.
#'
#' @details The locked actions can be specified in several different
#'   ways:
#'
#'   \describe{
#'
#'   \item{\code{integer}}{\code{vector} of indices pertaining to which
#'     actions should be locked in the solution (i.e. row numbers of the
#'     actions in the argument to \code{actions} in \code{\link{problem}}).}
#'
#'   \item{\code{logical}}{\code{vector} containing \code{logical}
#'     (i.e. \code{TRUE} and/or \code{FALSE} values) that indicate which
#'     actions should be locked in the solution. These \code{logical}
#'     values should correspond to each row in the argument to \code{actions}
#'     in \code{\link{problem}}).}
#'
#'   \item{\code{character}}{column name that indicates if actions
#'     units should be locked in the solution. This argument
#'     should  denote a column in the argument to \code{actions}
#'     in \code{\link{problem}} which contains \code{logical}
#'     (i.e. \code{TRUE} and/or \code{FALSE} values) to indicate
#'     which actions should be locked.}
#'  }
#'
#' @return \code{\link{ProjectProblem-class}} object with the constraints
#'   added to it.
#'
#' @seealso \code{\link{constraints}}.
#'
#' @examples
#'
#' @name add_locked_in_constraints
#'
#' @exportMethod add_locked_in_constraints
#'
#' @aliases add_locked_in_constraints,ProjectProblem,numeric-method add_locked_in_constraints,ProjectProblem,logical-method add_locked_in_constraints,ProjectProblem,character-method
#'
#' @export
methods::setGeneric("add_locked_in_constraints",
                    signature = methods::signature("x", "locked_in"),
                    function(x, locked_in)
                      standardGeneric("add_locked_in_constraints"))

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ProjectProblem,numeric}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ProjectProblem", "numeric"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ProjectProblem"),
      inherits(locked_in, c("integer", "numeric")),
      isTRUE(all(is.finite(locked_in))),
      isTRUE(all(round(locked_in) == locked_in)),
      isTRUE(max(locked_in) <= number_actions(x)),
      isTRUE(min(locked_in) >= 1))
    # add constraints
    add_manual_locked_constraints(x,
      data.frame(action = x$action_names()[locked_in], status = TRUE))
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ProjectProblem,logical}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ProjectProblem", "logical"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ProjectProblem"),
      inherits(locked_in, "logical"),
      assertthat::noNA(locked_in))
      # add constraints
      add_locked_in_constraints(x, which(locked_in))
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ProjectProblem,character}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ProjectProblem", "character"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ProjectProblem"),
      assertthat::is.string(locked_in),
      assertthat::noNA(locked_in),
      assertthat::has_name(x$data$actions, locked_in),
      is.logical(x$data$actions[[locked_in]]),
      assertthat::noNA(x$data$actions[[locked_in]]))
    # add constraints
    add_locked_in_constraints(x, which(x$data$actions[[locked_in]]))
})
