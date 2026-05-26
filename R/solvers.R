#' @include Solver-class.R
NULL

#' Solvers
#'
#' Solvers specify the software and configuration used to generate solutions
#' for a project prioritization problem. By default, the best available exact
#' algorithm solver is used.
#'
#' @details
#' The following solvers can be used to generate solutions for a
#' project prioritization problem.
#'
#' \describe{
#'
#' \item{[add_default_solver()]}{
#' Add the best installed solver.
#' }
#'
#' \item{[add_gurobi_solver()]}{
#' Add a solver to generate solutions with the
#' [*Gurobi*](https://www.gurobi.com) software.
#' }
#'
#' \item{[add_highs_solver()]}{
#' Add a solver to generate solutions with the
#' [*HiGHS*](https://highs.dev/) software via the \pkg{highs} package.
#' }
#'
#' \item{[add_cbc_solver()]}{
#' Add a solver to generate solutions with the
#' [*CBC*](https://github.com/coin-or/Cbc) software via the \pkg{rcbc} package.
#' }
#'
#' \item{[add_rsymphony_solver()]}{
#' Add a solver to generate solutions with the
#' [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) software via the
#' \pkg{Rsymphony} package.
#' }
#'
#' \item{[add_lpsymphony_solver()]}{
#' Add a solver to generate solutions with the
#' [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) software via the
#' \pkg{lpsymphony} package.
#' }
#'
#' \item{[add_lpsolveapi_solver()]}{
#' Add a solver to generate solutions with the
#' [*lp_solve*](https://lpsolve.sourceforge.net/5.5/) software via the
#' \pkg{lpSolveAPI} package.
#' }
#'
#' \item{[add_heuristic_solver()]}{
#' Add a solver to generate solutions using a backwards heuristic algorithm.
#' }
#'
#' \item{[add_random_solver()]}{
#' Add a solver to generate solutions by randomly selecting actions for
#' funding.
#' }
#'
#' }
#'
#' @name solvers
#'
#' @family overviews
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem
#' p1 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions()
#'
#' # build another problem, with the default solver
#' p2 <- p1 %>% add_default_solver()
#'
#' # build another problem, with the gurobi solver
#' p3 <- p1 %>% add_gurobi_solver()
#'
#' # build another problem, with the highs solver
#' p4 <- p1 %>% add_highs_solver()
#'
#' # build another problem, with the cbc solver
#' p5 <- p1 %>% add_cbc_solver()
#'
#' # build another problem, with the Rsymphony solver
#' p6 <- p1 %>% add_rsymphony_solver()
#'
#' # build another problem, with the lpsymphony solver
#' p7 <- p1 %>% add_lpsymphony_solver()
#'
#' # build another problem, with the lpSolveAPI solver
#' p8 <- p1 %>% add_lpsolveapi_solver()
#'
#' # build another problem, with the heuristic solver
#' p9 <- p1 %>% add_heuristic_solver()
#'
#' # build another problem, with the random solver
#' p10 <- p1 %>% add_random_solver()
#'
#' # generate solutions using each of the solvers
#' s <- rbind(
#'   solve(p2), solve(p3), solve(p4), solve(p5), solve(p6), solve(p7),
#'   solve(p8), solve(p9), solve(p10)
#' )
#' s$solver <- c(
#'   "default", "gurobi", "highs", "cbc", "Rsymphony", "lpsymphony",
#'   "lpSolveAPI", "heuristic", "random"
#' )
#'
#' # print solutions
#' print(as.data.frame(s))
NULL
