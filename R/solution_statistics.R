#' @include internal.R
NULL

#' Solution statistics
#'
#' Calculate statistics to describe a solution to a project prioritization
#' problem.
#'
#' @param x [problem()] or [multi_problem()] object.
#'
#' @param solution [base::data.frame()] or
#' [tibble::tibble()] containing the solutions. Here,
#' rows correspond to different solutions and columns correspond to
#' different actions. Each column in the argument to `solution` should
#' be named according to a different action in `x`.
#' Cell values indicate if an action is funded in a given solution or not,
#' and should be either zero or one. Arguments to `solution` can
#' contain additional columns, though they will be ignored.
#'
#' @return
#' A [tibble::tibble()] containing the following columns.
#'
#' \describe{
#'
#' \item{`"cost"`}{
#' This column contains `numeric` values describing the cost of each solution.
#' }
#'
#' \item{`"obj"`}{
#' This column contains `numeric` values describing the objective value
#' for each solution. This is calculated using the objective function defined
#' for the argument to `x`. Note that if `x` is a [multi_problem()] object,
#' then an objective column will be created for each problem in `x`.
#' }
#'
#' \item{`x$project_names()`}{
#' These columns contain `logical` values that indicate if each
#' project had all of its actions selected for funding or not.
#' }
#'
#' \item{`x$feature_names()`}{
#' These columns contain `numeric` values that describe the expected outcome
#' for each feature based on the actions selected for funding.
#' }
#'
#' }
#'
#' @family evaluation
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # print project data
#' print(sim_projects)
#'
#' # print action data
#' print(sim_features)
#'
#' # print feature data
#' print(sim_actions)
#'
#' # build problem
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 400) %>%
#'   add_feature_weights("weight") %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
#' # create a table with some solutions
#' solutions <- data.frame(
#'   F1_action = c(0, 1, 1),
#'   F2_action = c(0, 1, 0),
#'   F3_action = c(0, 1, 1),
#'   F4_action = c(0, 1, 0),
#'   F5_action = c(0, 1, 1),
#'   baseline_action = c(1, 1, 1)
#' )
#'
#' # print the solutions
#' # the first solution only has the baseline action funded
#' # the second solution has every action funded
#' # the third solution has only some actions funded
#' print(solutions)
#'
#' # calculate statistics for the solutions
#' solution_statistics(p, solutions)
#' @export
solution_statistics <- function(x, solution) {
  UseMethod("solution_statistics")
}

#' @export
solution_statistics.ProjectProblem <- function(x, solution) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "ProjectProblem"),
    inherits(solution, "data.frame"),
    all(assertthat::has_name(solution, x$action_names()))
  )
  assertthat::assert_that(
    !is.Waiver(x$objective),
    msg = "`x` must have an objective added to it."
  )
  if (!inherits(solution, "tbl_df")) {
    solution <- tibble::as_tibble(solution)
  }
  # calculate cost and objective values
  out <- tibble::tibble(
    cost = rowSums(
      as.matrix(solution[, x$action_names()]) *
        matrix(
          x$action_costs(),
          byrow = TRUE,
          ncol = x$number_of_actions(),
          nrow = nrow(solution)
        )
    ),
    obj = x$objective$evaluate(x, solution[, x$action_names()])
  )
  # add in columns indicating if each project is funded or not
  out <- tibble::as_tibble(
    cbind(
      out,
      stats::setNames(
        as.data.frame(
          rcpp_funded_projects(
            x$pa_matrix(),
            as_Matrix(as.matrix(solution[, x$action_names()]), "dgCMatrix")
          ) > 0.5
        ),
        x$project_names()
      )
    )
  )
  # add in columns for feature persistence values
  out <- tibble::as_tibble(
    cbind(
      out,
      stats::setNames(
        as.data.frame(
          rcpp_expected_persistences(
            x$pa_matrix(),
            x$eof_matrix(),
            as_Matrix(diag(x$number_of_features()), "dgCMatrix"),
            as_Matrix(as.matrix(solution[, x$action_names()]), "dgCMatrix")
          )
        ),
        x$feature_names()
      )
    )
  )
  # return output
  out
}

#' @export
solution_statistics.MultiObjProjectProblem <- function(x, solution) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "MultiObjProjectProblem"),
    inherits(solution, "data.frame")
  )
  assertthat::assert_that(
    all(
      assertthat::has_name(
        solution,
        unlist(x$action_names(), recursive = TRUE, use.names = FALSE)
      )
    ),
    msg = "`solution` must have a column for each action in `x`."
  )
  assertthat::assert_that(
    all(vapply(x$problems, function(x) !is.Waiver(x$objective),  logical(1))),
    msg = "all problems in `x` must have an objective added to them."
  )
  if (!inherits(solution, "tbl_df")) {
    solution <- tibble::as_tibble(solution)
  }
  # calculate solution statistics for first problem
  ## (note this includes adding a cost column)
  out <- solution_statistics(x$problems[[1]], solution)
  ## rename the objective column based on the name of the first problem
  names(out)[[2]] <- names(x$problems)[[1]]
  # calculate solution statistics for remaining problems and add them to output
  ## (note this does not add duplicate cost columns)
  for (i in seq_along(x$problems)[-1]) {
    curr_stats <- solution_statistics(
      x$problems[[i]], solution
    )[, -1, drop = FALSE]
    names(curr_stats)[[1]] <- names(x$problems)[[i]]
    out <- tibble::as_tibble(cbind(out, curr_stats))
  }
  # reorder columns
  out <- out[, c(
    "cost",
    x$problem_names(),
    unlist(x$project_names(), recursive = TRUE, use.names = FALSE),
    unlist(x$feature_names(), recursive = TRUE, use.names = FALSE)
  )]
  # return output
  out
}
