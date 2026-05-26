#' @include internal.R Constraint-class.R
NULL

#' Add locked in project constraints
#'
#' Add constraints to a project prioritization problem to ensure
#' that particular projects are selected for funding by the solution. For
#' example, it may be desirable to lock in projects for conserving culturally or
#' taxonomically important species.
#'
#' @param x [problem()] object.
#'
#' @param locked_in Object that determines which planning units that should be
#' locked in. See the Details section for more information.
#'
#' @details
#' The locked projects can be specified in several different ways:
#'
#' \describe{
#'
#' \item{`integer` vector}{
#' Each values specifies the index for a project that should be locked when
#' generating solutions
#' (i.e., row numbers of the projects in the argument to `projects` in
#' [problem()]).
#' }
#'
#' \item{`logical` vector}{
#' Each value (i.e., `TRUE` and/or `FALSE`) indicates
#' if a project should be locked (or not) when generating the solution.
#' These `logical` values should correspond to each row in the argument to
#' `projects` in [problem()].
#' }
#'
#' \item{`character` value}{
#' The value specifies the name of a column in the project data
#' (i.e., argument to `projects` in [problem()]). The column must
#' have `logical` (i.e., `TRUE` and/or `FALSE`) values, and these
#' values are used to indicate which projects are locked (or not).
#' }
#'
#' }
#'
#' @inherit add_locked_in_action_constraints return seealso
#'
#' @family constraints
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # add column to the project data to indicate which projects should be
#' # locked. in particular, this column will lock the first project
#' sim_projects$locked_in <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
#'
#' # print project data
#' print(sim_projects)
#'
#' # build problem with maximum weighted sum objective and $150 budget
#' p1 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 150) %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p1)
#'
#' # build another problem, and lock in the 3rd project using numeric inputs
#' p2 <- p1 %>% add_locked_in_project_constraints(c(3))
#'
#' # print problem
#' print(p2)
#'
#' # build another problem, and lock in the projects using logical inputs from
#' # the sim_projects stable
#' p3 <- p1 %>% add_locked_in_project_constraints(sim_projects$locked_in)
#'
#' # print problem
#' print(p3)
#'
#' # build another problem, and lock in the projects using the column name
#' # "locked_in" in the sim_projects table
#' p4 <- p1 %>% add_locked_in_project_constraints("locked_in")
#'
#' # print problem
#' print(p4)
#'
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#' s4 <- solve(p4)
#'
#' # print the projects selected for funding in each of the solutions
#' print(s1[, sim_projects$name])
#' print(s2[, sim_projects$name])
#' print(s3[, sim_projects$name])
#' print(s4[, sim_projects$name])
#' @name add_locked_in_project_constraints
#'
#' @exportMethod add_locked_in_project_constraints
#'
#' @aliases add_locked_in_project_constraints,ProjectProblem,numeric-method add_locked_in_project_constraints,ProjectProblem,logical-method add_locked_in_project_constraints,ProjectProblem,character-method
#'
#' @export
methods::setGeneric(
  "add_locked_in_project_constraints",
  signature = methods::signature("x", "locked_in"),
  function(x, locked_in) standardGeneric("add_locked_in_project_constraints")
)

#' @name add_locked_in_project_constraints
#' @usage \S4method{add_locked_in_project_constraints}{ProjectProblem,numeric}(x, locked_in)
#' @rdname add_locked_in_project_constraints
methods::setMethod(
  "add_locked_in_project_constraints",
  methods::signature("ProjectProblem", "numeric"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      inherits(locked_in, c("integer", "numeric")),
      isTRUE(all(is.finite(locked_in))),
      isTRUE(all(round(locked_in) == locked_in)),
      isTRUE(max(locked_in) <= number_of_projects(x)),
      isTRUE(min(locked_in) >= 1)
    )
    # add constraints
    add_manual_locked_project_constraints(
      x,
      data.frame(project = x$project_names()[locked_in], status = 1)
    )
  }
)

#' @name add_locked_in_project_constraints
#' @usage \S4method{add_locked_in_project_constraints}{ProjectProblem,logical}(x, locked_in)
#' @rdname add_locked_in_project_constraints
methods::setMethod(
  "add_locked_in_project_constraints",
  methods::signature("ProjectProblem", "logical"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      inherits(locked_in, "logical"),
      assertthat::noNA(locked_in),
      length(locked_in) == x$number_of_projects()
    )
    # add constraints
    add_locked_in_project_constraints(x, which(locked_in))
  }
)

#' @name add_locked_in_project_constraints
#' @usage \S4method{add_locked_in_project_constraints}{ProjectProblem,character}(x, locked_in)
#' @rdname add_locked_in_project_constraints
methods::setMethod(
  "add_locked_in_project_constraints",
  methods::signature("ProjectProblem", "character"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      assertthat::is.string(locked_in),
      assertthat::noNA(locked_in),
      assertthat::has_name(x$data$projects, locked_in),
      is.logical(x$data$projects[[locked_in]]),
      assertthat::noNA(x$data$projects[[locked_in]])
    )
    # add constraints
    add_locked_in_project_constraints(x, which(x$data$projects[[locked_in]]))
  }
)
