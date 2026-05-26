#' @include internal.R MultiObjProjectProblem-class.R ProjectProblem-class.R assertions.R
NULL

#' Multi-objective project prioritization problem
#'
#' Create a multi-objective systematic project prioritization problem.
#'
#' @param ... [problem()] objects.
#'
#' @param problem_names `character` vector with a name for each problem
#' in `...`. Defaults to `NULL`, such that the problem names are defined
#' automatically.
#'
#' @details
#' A multi-objective project prioritization problem contains multiple
#' single-objective project prioritization problems (i.e., created with
#' [problem()]). Each of these single-objective project prioritization problems
#' must have exactly the same actions (i.e., argument to `actions`).
#' Additionally, each single-objective project prioritization problem
#' must have a different set of projects and features (i.e., they have
#' have different names).
#'
#' @return A [MultiObjProjectProblem-class] object.
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_multi_projects)
#' data(sim_multi_features)
#' data(sim_multi_actions)
#' data(sim_multi_tree)
#'
#' # build problem
#' p <-
#'   multi_problem(
#'     obj1 =
#'       problem(
#'         sim_multi_projects[[1]], sim_multi_actions, sim_multi_features[[1]],
#'         "name", "success", "name", "cost", "name",
#'         baseline_project_name = "baseline_project_obj1"
#'       ) %>%
#'       add_max_phylo_div_objective(
#'        budget = 200, tree = sim_multi_tree[[1]]
#'       ) %>%
#'       add_binary_decisions(),
#'    obj2 =
#'      problem(
#'        sim_multi_projects[[2]], sim_multi_actions, sim_multi_features[[2]],
#'        "name", "success", "name", "cost", "name",
#'        baseline_project_name = "baseline_project_obj2"
#'      ) %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions(),
#'    obj3 =
#'      problem(
#'        sim_multi_projects[[3]], sim_multi_actions, sim_multi_features[[3]],
#'        "name", "success", "name", "cost", "name",
#'        baseline_project_name = "baseline_project_obj3"
#'      ) %>%
#'      add_max_wtd_sum_objective(budget = 200) %>%
#'      add_binary_decisions()
#'  ) %>%
#'  add_ref_point_approach(weights = c(10, 11, 12), goals = c(3, 4, 5)) %>%
#'  add_default_solver()
#'
#' # print problem
#' print(p)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#' @export
multi_problem <- function(..., problem_names = NULL) {
  # parse arguments
  x <- list(...)

  # if needed, create default names
  if (is.null(names(x)) && is.null(problem_names)) {
    problem_names <- paste("Problem", seq_along(x))
  }

  # if needed, ensure that no duplicate names
  if (!is.null(names(x))) {
    ## assert arguments are valid
    assertthat::assert_that(
      is.character(names(x)),
      msg = "names of `...` must be `character` values."
    )
    assertthat::assert_that(
      assertthat::noNA(names(x)),
      msg = "names of `...` must not have missing (`NA`) values."
    )
    assertthat::assert_that(
      identical(anyDuplicated(x), 0L),
      msg = "names of `...` must not have duplicated values."
    )
  }

  # if need, assign names
  if (is.null(names(x)) && !is.null(problem_names)) {
    ## assert arguments are valid
    assertthat::assert_that(
      is.character(problem_names),
      assertthat::noNA(problem_names),
      identical(anyDuplicated(problem_names), 0L)
    )
    assertthat::assert_that(
      identical(length(problem_names), length(x)),
      msg = "`problem_names` must have a value for each object in `...`."
    )
    ## assign names
    names(x) <- problem_names
  }

  # assert that arguments are valid
  assertthat::assert_that(
    length(x) >= 2,
    msg = "`...` must contain at least two `problem()` objects."
  )
  assertthat::assert_that(
    all(vapply(x, inherits, FUN.VALUE = logical(1), "ProjectProblem")),
    msg = "`...` must contain only `problem()` objects."
  )

  # assert that each object has exactly the same actions
  assertthat::assert_that(
    all(
      vapply(
        x, FUN.VALUE = logical(1),
        function(y) identical(x[[1]]$action_names(), y$action_names())
      )
    ),
    msg = "`...` must contain objects that all have exactly the same actions."
  )
  assertthat::assert_that(
    all(
      vapply(
        x, FUN.VALUE = logical(1),
        function(y) {
          identical(
            x[[1]]$data$actions[[x[[1]]$data$action_name_column]],
            y$data$actions[[y$data$action_name_column]]
          )
        }
      )
    ),
    msg = paste(
      "`...` must contain objects that all have exactly the same",
      "action names."
    )
  )
  assertthat::assert_that(
    all(
      vapply(
        x, FUN.VALUE = logical(1),
        function(y) {
          identical(
            x[[1]]$data$actions[[x[[1]]$data$action_cost_column]],
            y$data$actions[[y$data$action_cost_column]]
          )
        }
      )
    ),
    msg = paste(
      "`...` must contain objects that all have exactly the same",
      "action costs."
    )
  )

  # assert that each object has different features
  assertthat::assert_that(
    identical(
      anyDuplicated(
        unlist(
          lapply(x, function(y) y$feature_names()),
          recursive = FALSE, use.names = FALSE
        )
      ),
      0L
    ),
    msg = paste(
      "`...` must contain objects that all have different feature names."
    )
  )

  # assert that each object has different projects
  assertthat::assert_that(
    identical(
      anyDuplicated(
        unlist(
          lapply(x, function(y) y$project_names()),
          recursive = FALSE, use.names = FALSE
        )
      ),
      0L
    ),
    msg = paste(
      "`...` must contain objects that all have different project names."
    )
  )
  # if any of input problems have a non default solver specified, throw warning
  if (
    any(
      vapply(x, FUN.VALUE = logical(1), function(x) {
        !isTRUE(x$defaults$solver)
      })
    )
  ) {
    warning(
      "solvers specified for `...` input problems will be ignored.",
      immediate. = TRUE
    )
  }

  # if needed, set default problem names
  if (is.null(names(x))) {
    names(x) <- paste("Objective", seq_along(x)) # nocov
  }

  # create new multi objective conservation problem
  p <- MultiObjProjectProblem$new(problems = x)

  # add defaults
  p <- suppressWarnings(add_default_solver(p))
  p$defaults$solver <- TRUE

  # return result
  p
}
