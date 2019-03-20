#' @include internal.R Constraint-proto.R
NULL

#' Add locked out constraints
#'
#' Add constraints to a project prioritization \code{\link{problem}} to ensure
#' that specific actions are not prioritized for funding in the solution. For
#' example, it may be desirable to lock out specific actions to examine their
#' importance to the optimal funding scheme.
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
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # update "locked_out" column to lock out action "F2_action"
#' sim_actions$locked_out <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
#'
#' # print action data
#' print(sim_actions)
#'
#' # build problem with maximum richness objective and $150 budget
#' p1 <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 150) %>%
#'      add_binary_decisions()
#'
#' # print problem
#' print(p1)
#'
#' # build another problem, and lock out the second action using numeric inputs
#' p2 <- p1 %>%
#'       add_locked_out_constraints(c(2))
#'
#' # print problem
#' print(p2)
#'
#' # build another problem, and lock out the actions using logical inputs
#' # (i.e. TRUE/FALSE values) from the sim_actions table
#' p3 <- p1 %>%
#'       add_locked_out_constraints(sim_actions$locked_out)
#'
#' # print problem
#' print(p3)
#'
#' # build another problem, and lock out the actions using the column name
#' # "locked_out" in the sim_actions table
#' # the sim_actions table
#' p4 <- p1 %>%
#'       add_locked_out_constraints("locked_out")
#'
#' # print problem
#' print(p4)
#'
#' \donttest{
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#' s4 <- solve(p4)
#'
#' # print the actions selected for funding in each of the solutions
#' print(s1[, sim_actions$name])
#' print(s2[, sim_actions$name])
#' print(s3[, sim_actions$name])
#' print(s4[, sim_actions$name])
#' }
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
      isTRUE(max(locked_out) <= number_of_actions(x)),
      isTRUE(min(locked_out) >= 1))
    # add constraints
    add_manual_locked_constraints(x,
      data.frame(action = x$action_names()[locked_out], status = 0))
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
      assertthat::noNA(locked_out),
      length(locked_out) == x$number_of_actions())
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
