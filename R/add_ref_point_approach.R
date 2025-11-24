#' @include internal.R MultiObjApproach-class.R
NULL

#' Add a reference point approach
#'
#' Add a reference point approach for multi-objective optimization to a
#' project problem.
#'
#' @param x [multi_problem()] object.
#'
#' @param weights `numeric` vector containing the weights for each
#' objective. To generate multiple solutions based on different values,
#' `weights` can be a `numeric` matrix where
#' each row corresponds to a different solution and each columns
#' corresponds to a different objective.
#'
#' @param goals `numeric` vector containing values that denote the
#' reference points. These points represent aspirational goals for each
#' objective. To generate multiple solutions based on different values,
#' `goals` can be a `numeric` matrix where
#' each row corresponds to a different solution and each columns
#' corresponds to a different objective.
#'
#' @param method `character` value denoting the name of the formulation
#' used to calculate the overall performance of a solution across multiple
#' objectives. Available options include calculating the
#' (`"sum"`) weighted sum of the goal shortfalls and
#' ("max") weighted maximum of the goal shortfalls.
#' Defaults to `"sum"`.
#'
#' @param verbose `logical` should progress on generating solutions
#' displayed? Defaults to `TRUE`.
#'
#' @details
#' The reference point approach for multi-objective optimization involves
#' creating a new objective that is calculated based on multiple objectives.
#' In particular, the new objective uses weights to specify the relative
#' importance of each individual objective, and goals to specify
#' a threshold minimum level of performance for each objective
#' (conceptually similar to target thresholds used in conservation planning).
#' This function supports two methods for formulating a new objective
#' based on multiple objectives. Below we provide their mathematical details.
#'
#' To describe this approach mathematically, we will define the
#' following terminology.
#' Let \eqn{O} denote the set of objectives (indexed by \eqn{o}).
#' For each objective, let \eqn{W_o}{W_o} denote the weight goal each objective
#' \eqn{o \in O}{o in O}, \eqn{G_o}{G_o} denote the goal each objective
#' \eqn{o \in O}{o in O}, and \eqn{V_o}{V_o} denote the objective value
#' for a candidate solution as measured based on each objective
#' \eqn{o \in O}{o in O}. After defining these terms, we can define the
#' methods for formulating a new objective based on multiple objectives.
#'
#' One method involves creating a new objective that is based on calculating
#' the weighted sum of the goal shortfalls (per `method = "sum"`).
#' This method is based on the following equation.
#'
#' \deqn{
#' \mathrm{Minimize} \space \sum_{o = 0}^{O} W_o \times \frac{V_o}{W_o}
#' }{
#' Minimize sum_o^O W_o * (V_o / W_o)
#' }
#'
#' Another method involves creating a new objective that is based on calculating
#' the weighted maximum of the goal shortfalls (per `method = "max"`).
#' This method is based on the following equation.
#'
#' \deqn{
#' \mathrm{Minimize} \space \max_{o = 0}^{O} W_o \times \frac{V_o}{W_o}
#' }{
#' Minimize max_o^O W_o * (V_o / W_o)
#' }
#'
#' @return
#' A [multi_problem()] object with the approach added to it.
#'
#' @family approaches
#'
#' @examples
#' \dontrun{
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
#' }
#' @export
add_ref_point_approach <- function(x, weights, goals, method = "sum",
                                   verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "MultiObjProjectProblem"),
    assertthat::is.string(method),
    assertthat::noNA(method),
    method %in% c("sum", "max"),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  if (is.numeric(weights) && !is.matrix(weights)) {
    weights <- matrix(weights, nrow = 1)
  }
  if (is.numeric(goals) && !is.matrix(goals)) {
    goals <- matrix(goals, nrow = 1)
  }
  assertthat::assert_that(
    is.matrix(weights),
    ncol(weights) == number_of_problems(x),
    nrow(weights) >= 1,
    assertthat::noNA(c(weights))
  )
  assertthat::assert_that(
    is.matrix(goals),
    ncol(goals) == number_of_problems(x),
    nrow(goals) >= 1,
    assertthat::noNA(c(goals))
  )
  assertthat::assert_that(
    all(
      vapply(
        x$problems, FUN.VALUE = logical(1), function(y) {
          startsWith(class(y$objective)[[1]], "Max")
        }
      )
    ),
    msg = paste(
      "this approach is not compatible with problems that",
      "have a minimization objective."
    )
  )
  # add approach
  x$add_approach(
    R6::R6Class(
      "ReferencePointApproach",
      inherit = MultiObjApproach,
      public = list(
        name = "reference point approach",
        data = list(
          weights = weights, goals = goals, method = method, verbose = verbose
        ),
        run = function(x, solver) {
          ## initialization
          weights <- self$get_data("weights")
          goals <- self$get_data("goals")
          verbose <- self$get_data("verbose")
          sols <- vector(mode = "list", length = nrow(weights))
          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            pb <- cli::cli_progress_bar(
              "Generating solutions", total = nrow(weights)
            )
          }
          ## main processing
          for (i in seq_len(nrow(weights))) {
            ### copy optimization problem
            mo <- x$opt$copy()
            ### convert to ref point problem formulation
            if (identical(method, "sum")) {
              rcpp_convert_ref_point_sum_method(
                mo$ptr, x$modelsense, x$obj,
                weights[i, ], goals[i, ]
              )
            } else {
              rcpp_convert_ref_point_max_method(
                mo$ptr, x$modelsense, x$obj,
                weights[i, ], goals[i, ]
              )
            }
            ### solve problem
            sols[[i]] <- solver$solve(mo)
            ## if needed, update progress bar
            if (isTRUE(verbose)) {
              cli::cli_progress_update(id = pb)
            }
          }
          ## if needed, clean up progress bar
          if (isTRUE(verbose)) {
            cli::cli_progress_done(id = pb)
          }
          ## prepare solutions for output
          n_sol <- sum(lengths(sols))
          out <- vector(mode = "list", length = n_sol)
          k <- 0
          for (i in seq_along(sols)) {
            for (j in seq_along(sols[[i]])) {
              k <- k + 1
              out[[k]] <- sols[[i]][[j]]
            }
          }
          ## return solutions
          out
        }
      )
    )$new()
  )
}
