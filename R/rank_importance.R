#' @include internal.R
NULL

#' Rank importance
#'
#' Calculate the rank importance for projects in a project
#' prioritization [problem()] (Jung *et al.* 2021). Projects associated
#' with a higher rank value are more irreplaceable, and may
#' need to be implemented sooner than those with lower values.
#'
#' @inheritParams replacement_costs
#'
#' @param ranks `integer` number to incremental ranks to evaluate
#'   importance. Defaults to 10.
#'
#' @param budgets `numeric` budget values for generating
#'   prioritizations at each increment. This parameter can be used instead
#'   of `ranks` to specify the number of incremental ranks and also
#'   the budget values that should be considered for each rank.
#'   Defaults to `NULL`.
#'
#' @param ... Arguments passed to [solve()].
#'
#' @details
#' This method involves generating a series of incremental prioritizations,
#' that start with relatively few projects selected and then iteratively
#' selecting additional projects. Projects that are selected at the
#' first increment are assigned the highest importance score and those
#' selected in subsequent increments are assigned lower importance
#' score. Missing (`NA`) values are assigned to
#'  projects which are not selected for funding in the specified solution.
#'
#' @return
#' A [tibble::tibble()] table containing the following columns.
#'
#' \describe{
#'
#' \item{`"project"`}{
#' `character` name of each project.
#' }
#'
#' \item{`"rank"`}{
#' `integer` rank where the project was first selected.
#' }
#'
#' \item{`"score"`}{
#' `numeric` importance score.
#' }
#'
#' }
#'
#' @references
#' Jung M, Arnell A, de Lamo X, et al. (2021) Areas of global importance for
#' conserving terrestrial biodiversity, carbon and water.
#' *Nature Ecology and Evolution*, **5**, 1499--1509.
#'
#' @family evaluation
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with maximum weighted sum objective and $400 budget
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 400) %>%
#'   add_feature_weights("weight") %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#'
#' # calculate rank importance values
#' r <- rank_importance(p, s)
#'
#' # print output
#' print(r)
#' @export
rank_importance <- function(x, solution, n = 1, ranks = 10,
                            budgets = NULL, ...) {
  UseMethod("rank_importance")
}

#' @export
rank_importance.ProjectProblem <- function(x, solution, n = 1, ranks = 10,
                                           budgets = NULL, ...) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "ProjectProblem"),
    inherits(solution, "data.frame"),
    all(assertthat::has_name(solution, x$project_names())),
    is.logical(as.matrix(solution[, x$project_names()])),
    assertthat::noNA(c(as.matrix(solution[, x$project_names()]))),
    assertthat::is.count(n),
    is.finite(n),
    n <= nrow(solution)
  )
  assertthat::assert_that(
    (!is.null(ranks) & is.null(budgets)) ||
    (is.null(ranks) & !is.null(budgets)),
    msg = "One of `ranks` and `budgets` must not be `NULL` (not both)."
  )
  assertthat::assert_that(
    !inherits(x$objective, "MinimumSetObjective"),
    msg = "`x` must have an objective with a `budget` parameter."
  )
  if (!is.null(budgets)) {
    assertthat::assert_that(
      is.numeric(budgets),
      assertthat::noNA(budgets),
      length(budgets) >= 1
    )
  }
  if (!is.null(ranks)) {
    assertthat::assert_that(
      assertthat::is.count(ranks),
      is.finite(ranks)
    )
    budgets <-
      seq(0, 1, length.out = ranks + 1)[-1] *
      solution_statistics(x, solution[n, ])$cost
  }
  if (!inherits(solution, "tbl_df")) {
    solution <- tibble::as_tibble(solution)
  }
  # identify projects that were selected
  all_project_names <- x$project_names()
  sel_project_names <- which(c(as.matrix(solution[n, all_project_names])) > 0.5)
  sel_project_names <- all_project_names[sel_project_names]
  # identify actions that were selected
  all_action_names <- x$action_names()
  sel_action_names <- which(c(as.matrix(solution[n, all_action_names])) > 0.5)
  sel_action_names <- all_action_names[sel_action_names]
  # copy the problem and apply locked out constraints
  x2 <- x$clone(deep = TRUE)
  ## identify projects and actions with zero cost values
  zero_cost_projects <- x$project_names()[x$project_costs() < 1e-6]
  zero_cost_actions <- x$action_names()[x$action_costs() < 1e-6]
  ## identify projects to lock out
  locked_projects <- x$project_names()
  locked_projects <- locked_projects[
    (!locked_projects %in% sel_project_names) &
    (!locked_projects %in% zero_cost_projects)
  ]
  ## identify actions to lock out
  locked_actions <- x$action_names()
  locked_actions <- locked_actions[
    (!locked_actions %in% sel_action_names) &
    (!locked_actions %in% zero_cost_actions)
  ]
  ## apply project locked out constraints
  if (length(locked_projects) > 0) {
    x2 <- add_locked_out_project_constraints(
      x2,
      locked_out = x$project_names() %in% locked_projects
    )
  }
  ## apply action locked out constraints
  if (length(locked_projects) > 0) {
    x2 <- add_locked_out_action_constraints(
      x2,
      locked_out = x$action_names() %in% locked_actions
    )
  }
  # perform incremental rank procedure
  out <- tibble::tibble(
    project = all_project_names, rank = NA_real_, score = NA_real_
  )
  for (i in seq_along(budgets)) {
    ## update the problem to override the budgetary constraint
    x2$objective$data$budget <- budgets[[i]]
    ## generate solution
    curr_sol <- solve(x2, ...)
    ## identify projects selected in the solution
    curr_project_names <-
      which(c(as.matrix(curr_sol[1, all_project_names])) > 0.5)
    curr_project_names <- all_project_names[curr_project_names]
    ## identify actions selected in the solution
    curr_action_names <-
      which(c(as.matrix(curr_sol[1, all_action_names])) > 0.5)
    curr_action_names <- all_action_names[curr_action_names]
    ## update the result with rank values
    out$rank[is.na(out$rank) & (out$project %in% curr_project_names)] <- i
    ## lock in selected projects for next iteration
    curr_locked_in_projects <- x2$project_names() %in% curr_project_names
    if (any(curr_locked_in_projects)) {
      x2 <- add_locked_in_project_constraints(
        x2, locked_in = curr_locked_in_projects
      )
    }
    ## lock in selected actions for next iteration
    curr_locked_in_actions <- x2$action_names() %in% curr_action_names
    if (any(curr_locked_in_actions)) {
      x2 <- add_locked_in_action_constraints(
        x2, locked_in = curr_locked_in_actions
      )
    }
    ## update starting solution
    x2$solver$set_start_solution(as.numeric(
      as.matrix(curr_sol[1, all_action_names]))
    )
  }
  # calculate scores
  out$score <- rescale(out$rank, from = c(1, length(budgets)), to = c(1, 0))
  # return output
  out
}

#' @export
rank_importance.MultiObjProjectProblem <- function(x, solution, n = 1,
                                                   ranks = 10, budgets = NULL,
                                                   ...) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "MultiObjProjectProblem"),
    inherits(solution, "data.frame"),
    all(assertthat::has_name(solution, unlist(x$project_names()))),
    is.logical(as.matrix(solution[, unlist(x$project_names())])),
    assertthat::noNA(as.matrix(solution[, unlist(x$project_names())])),
    assertthat::is.count(n),
    is.finite(n),
    n <= nrow(solution)
  )
  assertthat::assert_that(
    (!is.null(ranks) & is.null(budgets)) ||
    (is.null(ranks) & !is.null(budgets)),
    msg = "One of `ranks` and `budgets` must not be `NULL` (not both)."
  )
  if (!is.null(budgets)) {
    assertthat::assert_that(
      is.numeric(budgets),
      assertthat::noNA(budgets),
      length(budgets) >= 1
    )
  }
  if (!is.null(ranks)) {
    assertthat::assert_that(
      assertthat::is.count(ranks),
      is.finite(ranks)
    )
    budgets <-
      seq(0, 1, length.out = ranks + 1)[-1] *
      solution_statistics(x, solution[n, ])$cost
  }
  if (!inherits(solution, "tbl_df")) {
    solution <- tibble::as_tibble(solution)
  }
  # determine budget objective to override
  is_budget_obj <- !vapply(
    x$problems,
    function(x) inherits(x$objective, "MinimumSetObjective"),
    logical(1)
  )
  assertthat::assert_that(
    any(is_budget_obj),
    msg =
      "At least one of the objectives in `x` must have a `budget` parameter."
  )
  # identify projects that were selected
  all_project_names <- unlist(
    x$project_names(), use.names = FALSE, recursive = TRUE
  )
  sel_project_names <- which(c(as.matrix(solution[n, all_project_names])) > 0.5)
  sel_project_names <- all_project_names[sel_project_names]
  # identify actions that were selected
  all_action_names <- x$action_names()
  sel_action_names <- which(c(as.matrix(solution[n, all_action_names])) > 0.5)
  sel_action_names <- all_action_names[sel_action_names]
  # copy the problem and apply locked out constraints
  x2 <- x$clone(deep = TRUE)
  for (i in seq_along(x2$problems)) {
    ## identify projects and actions with zero cost values
    curr_zero_cost_projects <- x$problems[[i]]$project_names()[
      x$problems[[i]]$project_costs() < 1e-6
    ]
    curr_zero_cost_actions <- x$problems[[i]]$action_names()[
      x$problems[[i]]$action_costs() < 1e-6
    ]
    ## identify projects to lock out
    curr_locked_projects <- x$project_names()[[i]]
    curr_locked_projects <- curr_locked_projects[
      (!curr_locked_projects %in% sel_project_names) &
      (!curr_locked_projects %in% curr_zero_cost_projects)
    ]
    ## identify actions to lock out
    curr_locked_actions <- x$action_names()[[i]]
    curr_locked_actions <- curr_locked_actions[
      (!curr_locked_actions %in% sel_action_names) &
      (!curr_locked_actions %in% curr_zero_cost_actions)
    ]
    ## apply project locked out constraints
    if (length(curr_locked_projects) > 0) {
      x2$problems[[i]] <- add_locked_out_project_constraints(
        x2$problems[[i]],
        locked_out = x2$problems[[i]]$project_names() %in% curr_locked_projects
      )
    }
    ## apply action locked out constraints
    if (length(curr_locked_actions) > 0) {
      x2$problems[[i]] <- add_locked_out_action_constraints(
        x2$problems[[i]],
        locked_out = x2$problems[[i]]$action_names() %in% curr_locked_actions
      )
    }
  }
  # perform incremental rank procedure
  out <- tibble::tibble(
    project = all_project_names, rank = NA_real_, score = NA_real_
  )
  for (i in seq_along(budgets)) {
    ## update the problem to override the budgetary constraint
    idx <- which(is_budget_obj)[[1]]
    x2$problems[[idx]]$objective$data$budget <- budgets[[i]]
    ## generate solution
    curr_sol <- solve(x2, ...)
    ## identify projects selected in the solution
    curr_project_names <-
      which(c(as.matrix(curr_sol[1, all_project_names])) > 0.5)
    curr_project_names <- all_project_names[curr_project_names]
    ## identify actions selected in the solution
    curr_action_names <-
      which(c(as.matrix(curr_sol[1, all_action_names])) > 0.5)
    curr_action_names <- all_action_names[curr_action_names]
    ## update the result with rank values
    out$rank[is.na(out$rank) & out$project %in% curr_project_names] <- i
    ## update problem for next iteration
    for (i in seq_along(x2$problems)) {
      ## lock in selected projects
      curr_locked_in_projects <-
        x2$problems[[i]]$project_names() %in% curr_project_names
      if (any(curr_locked_in_projects)) {
        x2$problems[[i]] <- add_locked_in_project_constraints(
          x2$problems[[i]],
          locked_in = curr_locked_in_projects
        )
      }
      ## lock in selected actions
      curr_locked_in_actions <-
        x2$action_names() %in% curr_action_names
      if (any(curr_locked_in_actions)) {
        x2$problems[[i]] <- add_locked_in_action_constraints(
          x2$problems[[i]],
          locked_in = curr_locked_in_actions
        )
      }
    }
    ## update starting solution
    x2$solver$set_start_solution(as.numeric(
      as.matrix(curr_sol[1, all_action_names])
    ))
  }
  # calculate scores
  out$score <- rescale(out$rank, from = c(1, length(budgets)), to = c(1, 0))
  # return output
  out
}

#' Rescale numbers
#'
#' Linearly rescale numbers from one domain to another.
#'
#' @param x `numeric` vector of numbers to rescale.
#'
#' @param from `numeric` vector of two numbers representing the minimum
#' maximum numbers that are currently for `x`.
#'
#' @param to `numeric` vector of two numbers representing the new minimum
#' maximum numbers that `x` should be rescaled to.
#'
#' @noRd
rescale <- function(x, from, to) {
  # assert valid arguments
  assertthat::assert_that(
    is.numeric(x),
    is.numeric(from),
    is.numeric(to),
    assertthat::noNA(from),
    assertthat::noNA(to),
    length(from) == 2,
    length(to) == 2
  )
  # obtained from scales::rescale()
  (x - from[1])/diff(from) * diff(to) + to[1]
}
