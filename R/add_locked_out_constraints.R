#' @include internal.R Constraint-proto.R
NULL

#' Add locked out constraints
#'
#' Add constraints to a project prioritization \code{\link{problem}} to ensure
#' that specific actions are not funded in the solution. For example, it may be
#' desirable to lock out specific actions to examine their importance to the
#' optimal funding scheme. If specific actions should be locked in to a
#' solution, use \code{\link{add_locked_in_constraints}}.
#'
#' @usage add_locked_out_constraints(x, locked_out)
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param locked_out Object that determines which planning units that should be
#'   locked out. See the Details section for more information.
#'
#' @inherit add_locked_out_constraints details seealso return
#'
#' @examples
#' #TODO
#'
#' @name add_locked_out_constraints
#'
#' @exportMethod add_locked_out_constraints
#'
#' @aliases add_locked_out_constraints,ProjectProblem,numeric-method add_locked_out_constraints,ProjectProblem,logical-method add_locked_out_constraints,ProjectProblem,character-method
#'
#' @export
methods::setGeneric("add_locked_out_constraints",
                    signature = methods::signature("x", "locked_out"),
                    function(x, locked_out)
                      standardGeneric("add_locked_out_constraints"))

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ProjectProblem,numeric}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ProjectProblem", "numeric"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ProjectProblem"),
      inherits(locked_out, c("integer", "numeric")),
      isTRUE(all(is.finite(locked_out))),
      isTRUE(all(round(locked_out) == locked_out)),
      isTRUE(max(locked_out) <= number_actions(x)),
      isTRUE(min(locked_out) >= 1))
    # add constraints
    add_manual_locked_constraints(x,
      data.frame(action = x$action_names()[locked_out], status = FALSE))
})

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ProjectProblem,logical}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ProjectProblem", "logical"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ProjectProblem"),
      inherits(locked_out, "logical"),
      assertthat::noNA(locked_out))
      # add constraints
      add_locked_out_constraints(x, which(locked_out))
})

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ProjectProblem,character}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ProjectProblem", "character"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ProjectProblem"),
      assertthat::is.string(locked_out),
      assertthat::noNA(locked_out),
      assertthat::has_name(x$data$actions, locked_out),
      is.logical(x$data$actions[[locked_out]]),
      assertthat::noNA(x$data$actions[[locked_out]]))
    # add constraints
    add_locked_out_constraints(x, which(x$data$actions[[locked_out]]))
})
