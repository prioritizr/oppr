#' @include internal.R ProjectProblem-class.R
NULL

#' Add manually specified locked constraints
#'
#' Add constraints to a project prioritization [problem()] to ensure
#' that solutions fund (or do not fund) specific actions. This function offers
#' more fine-grained control than the [add_locked_in_constraints()]
#' and [add_locked_out_constraints()] functions.
#'
#' @inheritParams add_locked_in_constraints
#'
#' @param locked `data.frame` or [tibble::tibble()] object. See
#'   the Details section for more information.
#'
#' @details
#' The argument to `locked` must contain the following columns:
#'
#' \describe{
#'
#' \item{`"action"`}{`character` values with action names.}
#'
#' \item{`"status"`}{
#' `numeric` values indicating if actions should
#'  be selected for funding (with a value of 1) or not (with a value of zero).
#' }
#'
#' }
#'
#' @inherit add_locked_in_constraints return seealso
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # create data frame with locked statuses
#' status <- data.frame(
#'   action = sim_actions$name[1:2],
#'   status = c(0, 1)
#' )
#'
#' # print locked statuses
#' print(status)
#'
#' # build problem with minimum set objective and targets that require each
#' # feature to have a 30% chance of persisting into the future
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 500) %>%
#'   add_manual_locked_constraints(status) %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
#' \dontrun{
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#' }
#'
#' @seealso [constraints].
#'
#' @name add_manual_locked_constraints
#'
#' @exportMethod add_manual_locked_constraints
#'
#' @aliases add_manual_locked_constraints,ProjectProblem,data.frame-method add_manual_locked_constraints,ProjectProblem,tbl_df-method
#'
#' @export
methods::setGeneric(
  "add_manual_locked_constraints",
  signature = methods::signature("x", "locked"),
  function(x, locked) standardGeneric("add_manual_locked_constraints")
)

#' @name add_manual_locked_constraints
#' @usage \S4method{add_manual_locked_constraints}{ProjectProblem,data.frame}(x, locked)
#' @rdname add_manual_locked_constraints
methods::setMethod("add_manual_locked_constraints",
  methods::signature("ProjectProblem", "data.frame"),
  function(x, locked) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      inherits(locked, "data.frame")
    )
    # add constraints
    add_manual_locked_constraints(x, tibble::as_tibble(locked))
  }
)

#' @name add_manual_locked_constraints
#' @usage \S4method{add_manual_locked_constraints}{ProjectProblem,tbl_df}(x, locked)
#' @rdname add_manual_locked_constraints
methods::setMethod("add_manual_locked_constraints",
  methods::signature("ProjectProblem", "tbl_df"),
  function(x, locked) {
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      inherits(locked, "tbl_df"),
      nrow(locked) > 0,
      assertthat::has_name(locked, "action"),
      inherits(locked$action, c("character", "factor")),
      assertthat::noNA(locked$action),
      all(locked$action %in%
          as.character(x$action_names())),
      assertthat::has_name(locked, "status"),
      is.numeric(locked$status),
      all(locked$status %in% c(0, 1)),
      assertthat::noNA(locked$status)
    )
    # set attributes
    if (all(locked$status == 1)) {
      class_name <- "LockedInConstraint"
      constraint_name <- "locked in actions"
    } else if (all(!locked$status == 0)) {
      class_name <- "LockedOutConstraint"
      constraint_name <- "locked out actions"
    } else {
      class_name <- "LockedManualConstraint"
      constraint_name <- "manually locked actions"
    }
     # add constraints
    x$add_constraint(
      R6::R6Class(
        class_name,
        inherit = Constraint,
        public = list(
          name = constraint_name,
          data = list(action_names = x$action_names(), locked = locked),
          apply = function(x, y) {
            assertthat::assert_that(
              inherits(x, "OptimizationProblem"),
              inherits(y, "ProjectProblem")
            )
            d <- self$get_data("locked")
            invisible(
              rcpp_apply_locked_constraints(
                x$ptr,
                match(d$action, self$data$action_names),
                as.integer(d$status)
              )
            )
          }
        )
      )$new()
    )
  }
)
