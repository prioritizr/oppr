#' @include internal.R Decision-class.R
NULL

#' Add binary decisions
#'
#' Add binary decisions to a project prioritization problem.
#' This means that the optimization process aims to determine if
#' each action should be selected for funding or not.
#'
#' @param x [problem()] object.
#'
#' @details
#' Project prioritization problems involve making decisions about
#' how funding will be allocated to management actions.
#' If no decision is added to a problem then this decision type will
#' be used by default. Currently, this is the only supported decision type.
#'
#' @return A [problem()] object with the decisions added to it.
#'
#' @family decisions
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with maximum weighted sum objective, $200 budget, and
#' # binary decisions
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
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
#'
#' # plot solution
#' plot(p, s)
#' @name add_binary_decisions
NULL

#' @rdname add_binary_decisions
#' @export
add_binary_decisions <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ProjectProblem"))
  # add decision
  x$add_decisions(
    R6::R6Class(
      "BinaryDecision",
      inherit = Decision,
      public = list(
        name = "binary decision",
        apply = function(x, y) {
          assertthat::assert_that(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ProjectProblem")
          )
          invisible(rcpp_apply_decisions(x$ptr, "B", 0, 1))
        }
      )
    )$new()
  )
}
