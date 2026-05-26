#' @include internal.R Constraint-class.R
NULL

#' Add locked out project constraints
#'
#' Add constraints to a project prioritization problem to ensure
#' that particular projects are not selected for funding by the solution.
#' For example, it may be desirable to lock out specific projects to examine
#' their importance to the optimal funding scheme.
#'
#' @inheritParams add_locked_in_project_constraints
#'
#' @param locked_out Object that determines which planning units that should be
#' locked out. See the Details section for more information.
#'
#' @inherit add_locked_out_project_constraints details seealso return
#'
#' @family constraints
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # add column to the project data to indicate which projects should be
#' # locked. in particular, this column will lock the first project
#' sim_projects$locked_out <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
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
#' # build another problem, and lock out the second project using numeric inputs
#' p2 <- p1 %>% add_locked_out_project_constraints(c(2))
#'
#' # print problem
#' print(p2)
#'
#' # build another problem, and lock out the projects using logical inputs
#' # (i.e., TRUE/FALSE values) from the sim_projects table
#' p3 <- p1 %>% add_locked_out_project_constraints(sim_projects$locked_out)
#'
#' # print problem
#' print(p3)
#'
#' # build another problem, and lock out the projects using the column name
#' # "locked_out" in the sim_projects table
#' p4 <- p1 %>% add_locked_out_project_constraints("locked_out")
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
#' @name add_locked_out_project_constraints
#'
#' @exportMethod add_locked_out_project_constraints
#'
#' @aliases add_locked_out_project_constraints,ProjectProblem,numeric-method add_locked_out_project_constraints,ProjectProblem,logical-method add_locked_out_project_constraints,ProjectProblem,character-method
#'
#' @export
methods::setGeneric(
  "add_locked_out_project_constraints",
  signature = methods::signature("x", "locked_out"),
  function(x, locked_out) standardGeneric("add_locked_out_project_constraints")
)

#' @name add_locked_out_project_constraints
#' @usage \S4method{add_locked_out_project_constraints}{ProjectProblem,numeric}(x, locked_out)
#' @rdname add_locked_out_project_constraints
methods::setMethod(
  "add_locked_out_project_constraints",
  methods::signature("ProjectProblem", "numeric"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      inherits(locked_out, c("integer", "numeric")),
      isTRUE(all(is.finite(locked_out))),
      isTRUE(all(round(locked_out) == locked_out)),
      isTRUE(max(locked_out) <= number_of_projects(x)),
      isTRUE(min(locked_out) >= 1)
    )
    # add constraints
    add_manual_locked_project_constraints(
      x,
      data.frame(project = x$project_names()[locked_out], status = 0)
    )
  }
)

#' @name add_locked_out_project_constraints
#' @usage \S4method{add_locked_out_project_constraints}{ProjectProblem,logical}(x, locked_out)
#' @rdname add_locked_out_project_constraints
methods::setMethod(
  "add_locked_out_project_constraints",
  methods::signature("ProjectProblem", "logical"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      inherits(locked_out, "logical"),
      assertthat::noNA(locked_out),
      length(locked_out) == x$number_of_projects()
    )
    # add constraints
    add_locked_out_project_constraints(x, which(locked_out))
  }
)

#' @name add_locked_out_project_constraints
#' @usage \S4method{add_locked_out_project_constraints}{ProjectProblem,character}(x, locked_out)
#' @rdname add_locked_out_project_constraints
methods::setMethod(
  "add_locked_out_project_constraints",
  methods::signature("ProjectProblem", "character"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      assertthat::is.string(locked_out),
      assertthat::noNA(locked_out),
      assertthat::has_name(x$data$projects, locked_out),
      is.logical(x$data$projects[[locked_out]]),
      assertthat::noNA(x$data$projects[[locked_out]])
    )
    # add constraints
    add_locked_out_project_constraints(x, which(x$data$projects[[locked_out]]))
  }
)
