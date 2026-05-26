#' @include Solver-class.R
NULL

#' Add a random solver
#'
#' Add a solver to generate solutions to a project prioritization problem
#' based on random selection. Although prioritizations should be developed
#' using optimization routines, a portfolio of randomly generated solutions can
#' be useful for evaluating the effectiveness of an optimized solution.
#'
#' @inheritParams add_gurobi_solver
#'
#' @details
#' The algorithm used to randomly generate solutions depends on the
#' the objective specified for the project prioritization problem.
#'
#' The following steps are used for objectives that maximize benefit subject to
#' budgetary constraints (e.g., [add_max_wtd_sum_objective()]).
#'
#' \enumerate{
#'
#' \item All locked in and zero-cost actions are initially
#' selected for funding (excepting actions which are locked out).
#'
#' \item A project---and all of its associated actions---is randomly selected
#' for funding (excepting projects associated with locked out actions,
#' and projects which would cause the budget to be exceeded when added
#' to the existing set of selected actions).
#'
#' \item The previous step is repeated until no more projects can be
#' selected for funding without the total cost of the prioritized actions
#'  exceeding the budget.
#'
#' }
#'
#' The following steps are used for objectives that  minimize cost subject to
#' biodiversity constraints (i.e., [add_min_set_objective()].
#'
#' \enumerate{
#'
#' \item All locked in and zero-cost actions are initially
#' selected for funding (excepting actions which are locked out).
#'
#' \item A project---and all of its associated actions---is randomly selected
#' for funding (excepting projects associated with locked out actions,
#' and projects which would cause the budget to be exceeded when added
#' to the existing set of selected actions).
#'
#' \item The previous step is repeated until all of the persistence targets
#' are met.
#'
#'  }
#'
#' @inherit add_gurobi_solver seealso return
#'
#' @family solvers
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with random solver, and generate 100 random solutions
#' p1 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions() %>%
#'   add_random_solver(number_solutions = 100)
#'
#' # print problem
#' print(p1)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solutions
#' print(s1)
#'
#' # plot first random solution
#' plot(p1, s1)
#'
#' # plot histogram of the objective values for the random solutions
#' # according to the weighted sum objective
#' hist(
#'   s1$obj,
#'   xlab = "Expected outcome", xlim = c(0, 2.5),
#'   main = "Histogram of random solutions"
#' )
#'
#' # since the objective values don't tell us much about the quality of the
#' # solutions, we can find the optimal solution and calculate how different
#' # each of the random solutions is from optimality
#'
#' # find the optimal objective value using an exact algorithms solver
#' s2 <- p1 %>%
#'   add_default_solver() %>%
#'   solve()
#'
#' # create new column in s1 with percent difference from optimality
#' s1$optimality_diff <- ((s2$obj - s1$obj) / s1$obj) * 100
#'
#' # plot histogram showing the quality of the random solutions
#' # higher numbers indicate worse solutions
#' hist(
#'   s1$optimality_diff,
#'   xlab = "Difference from optimality (%)",
#'   main = "Histogram of random solutions", xlim = c(0, 50)
#' )
#' @export
add_random_solver <- function(x, number_solutions = 1, verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "ProjectProblem"),
    assertthat::is.count(number_solutions),
    assertthat::noNA(number_solutions),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  # add solver
  x$add_solver(
    R6::R6Class(
      "RandomSolver",
      inherit = Solver,
      public = list(
        name = "random solver",
        data = list(
          number_solutions = number_solutions,
          verbose = verbose
        ),
        solve = function(x, ...) {
          # extract data
          locked_in <- which(x$lb()[seq_len(x$number_of_actions())] > 0.5)
          locked_out <- which(x$ub()[seq_len(x$number_of_actions())] < 0.5)
          # initialize variables for different objectives
          if (!is.Waiver(x$data$targets)) {
            targets <- x$data$targets$output()$value
          } else {
            fp <- x$data$feature_phylogeny()
            targets <- rep(0, length(fp$tip.label))
          }
          if (inherits(x$data$objective, "MinimumSetObjective")) {
            budget <- Inf
          } else {
            budget <- x$data$objective$get_data("budget")
          }
          # generate solutions
          runtime <- system.time({
            s <- rcpp_random_solution(
              x$data$action_costs(),
              x$data$pa_matrix(),
              x$data$eof_matrix(),
              targets,
              budget,
              locked_in, locked_out,
              self$get_data("number_solutions"),
              as.logical(self$get_data("verbose")),
              class(x$data$objective)[1]
            )
          })[[3]]
          # coerce logical to numeric matrix
          s <- round(s)
          # format solution data
          lapply(
            seq_len(nrow(s)), function(i) {
              list(
                x = s[i, , drop = TRUE],
                objective = NA_real_,
                status = NA_character_,
                runtime = runtime
              )
            }
          )
        }
      )
    )$new()
  )
}
