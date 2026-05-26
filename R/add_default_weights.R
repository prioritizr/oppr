#' @include Weight-class.R
NULL

#' Add default weights
#'
#' Add the default weights to a project prioritization problem.
#'
#' @param x [problem()] object.
#'
#' @family weights
#'
#' @inherit add_feature_weights return seealso
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with default solver
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_default_weights() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#'
#' # print problem
#' print(p)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#'
#' # plot solution
#' plot(p, s)
#' @export
add_default_weights <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"))
  add_feature_weights(x, x$objective$default_feature_weights())
}
