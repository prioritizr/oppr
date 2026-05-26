#' @include internal.R ProjectProblem-class.R
NULL

#' Add manually specified locked constraints for projects
#'
#' Add constraints to a project prioritization problem to ensure
#' that particular projects are selected, or not selected, for funding
#' by the solution. This function offers
#' more fine-grained control than the [add_locked_in_project_constraints()]
#' and [add_locked_out_project_constraints()] functions.
#'
#' @inheritParams add_locked_in_project_constraints
#'
#' @param locked `data.frame` or [tibble::tibble()] object. See
#' the Details section for more information.
#'
#' @details
#' The argument to `locked` must contain the following columns.
#'
#' \describe{
#'
#' \item{`"project"`}{`character` values with project names.}
#'
#' \item{`"status"`}{
#' `numeric` values indicating if projects should
#'  be selected for funding (with a value of 1) or not (with a value of zero).
#' }
#'
#' }
#'
#' @inherit add_locked_in_project_constraints return seealso
#'
#' @family constraints
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # create data frame with locked statuses
#' locked_data <- data.frame(
#'   project = sim_projects$name[1:2],
#'   status = c(0, 1)
#' )
#'
#' # print locked statuses
#' print(locked_data)
#'
#' # build problem with minimum set objective and targets that require each
#' # feature to have a 30% chance of persisting into the future
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 500) %>%
#'   add_manual_locked_project_constraints(locked_data) %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#' @name add_manual_locked_project_constraints
#'
#' @exportMethod add_manual_locked_project_constraints
#'
#' @aliases add_manual_locked_project_constraints,ProjectProblem,data.frame-method add_manual_locked_project_constraints,ProjectProblem,tbl_df-method
#'
#' @export
methods::setGeneric(
  "add_manual_locked_project_constraints",
  signature = methods::signature("x", "locked"),
  function(x, locked) standardGeneric("add_manual_locked_project_constraints")
)

#' @name add_manual_locked_project_constraints
#' @usage \S4method{add_manual_locked_project_constraints}{ProjectProblem,data.frame}(x, locked)
#' @rdname add_manual_locked_project_constraints
methods::setMethod(
  "add_manual_locked_project_constraints",
  methods::signature("ProjectProblem", "data.frame"),
  function(x, locked) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      inherits(locked, "data.frame")
    )
    # add constraints
    add_manual_locked_project_constraints(x, tibble::as_tibble(locked))
  }
)

#' @name add_manual_locked_project_constraints
#' @usage \S4method{add_manual_locked_project_constraints}{ProjectProblem,tbl_df}(x, locked)
#' @rdname add_manual_locked_project_constraints
methods::setMethod(
  "add_manual_locked_project_constraints",
  methods::signature("ProjectProblem", "tbl_df"),
  function(x, locked) {
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      inherits(locked, "tbl_df"),
      nrow(locked) > 0,
      assertthat::has_name(locked, "project"),
      inherits(locked$project, c("character", "factor")),
      assertthat::noNA(locked$project),
      all(locked$project %in% as.character(x$project_names())),
      assertthat::has_name(locked, "status"),
      is.numeric(locked$status),
      all(locked$status %in% c(0, 1)),
      assertthat::noNA(locked$status)
    )
    # set attributes
    if (all(locked$status == 1)) {
      class_name <- "LockedInConstraint"
      constraint_name <- "locked in projects"
    } else if (all(!locked$status == 0)) {
      class_name <- "LockedOutConstraint"
      constraint_name <- "locked out projects"
    } else {
      class_name <- "LockedManualConstraint"
      constraint_name <- "manually locked projects"
    }
    # add constraints
    x$add_constraint(
      R6::R6Class(
        class_name,
        inherit = Constraint,
        public = list(
          name = constraint_name,
          data = list(project_names = x$project_names(), locked = locked),
          apply = function(x, y) {
            assertthat::assert_that(
              inherits(x, "OptimizationProblem"),
              inherits(y, "ProjectProblem")
            )
            d <- self$get_data("locked")
            invisible(
              rcpp_apply_locked_project_constraints(
                x$ptr,
                match(d$project, self$data$project_names),
                as.integer(d$status)
              )
            )
          }
        )
      )$new()
    )
  }
)
