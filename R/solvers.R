#' @include Solver-proto.R
NULL

#' Problem solvers
#'
#' Specify the software and configuration used to solve a project prioritization
#' [problem()]. By default, the best available exact algorithm
#' solver will be used.
#'
#' @details The following solvers can be used to find solutions for a
#'   project prioritization [problem()]:
#'
#'   \describe{
#'
#'   \item{[add_default_solver()]}{This solver uses the best software
#'     currently installed on the system.}
#'
#' \item{[add_gurobi_solver()]}{
#'     [*Gurobi*](https://www.gurobi.com)
#'     is a state-of-the-art commercial optimization software with an R package
#'     interface. It is by far the fastest solver that can be used by
#'     this package, however, it is also the only solver that is not freely
#'     available. That said, licenses are available to academics at no cost. The
#'     \pkg{gurobi} package is distributed with the *Gurobi* software
#'     suite. This solver uses the \pkg{gurobi} package to solve problems.}
#'
#'   \item{[add_rsymphony_solver()]}{
#'     [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) is an
#'     open-source integer programming solver that is part of the Computational
#'     Infrastructure for Operations Research (COIN-OR) project, an initiative
#'     to promote development of open-source tools for operations research (a
#'     field that includes linear programming). The \pkg{Rsymphony} package
#'     provides an interface to COIN-OR and is available on CRAN. This solver
#'     uses the \pkg{Rsymphony} package to solve problems.}
#'
#'   \item{[add_lpsymphony_solver()]}{The \pkg{lpsymphony} package
#'     provides a different interface to the COIN-OR software suite. Unlike the
#'     \pkg{Rsymhpony} package, the \pkg{lpsymphony} package is distributed
#'     through
#'     [Bioconductor](https://doi.org/doi:10.18129/B9.bioc.lpsymphony).
#'     The \pkg{lpsymphony} package may be easier to install on Windows or
#'     Max OSX systems than the \pkg{Rsymphony} package.}
#'
#'   \item{[add_lpsolveapi_solver()]}{
#'     [*lp_solve*](https://lpsolve.sourceforge.net/5.5/) is an
#'     open-source integer programming solver. The \pkg{lpSolveAPI} package
#'     provides an interface to this solver and is available on CRAN.
#'     Although this solver is the slowest currently supported solver,
#'     it is also the only exact algorithm solver that can be installed on all
#'     operating systems without any manual installation steps.}
#'
#'   \item{[add_heuristic_solver()]}{Generate solutions using
#'     a backwards heuristic algorithm. Although these types of algorithms have
#'     conventionally been used to solve project prioritization problems,
#'     they are extremely unlikely to identify optimal solutions and provide
#'     no guarantees concerning solution quality.}
#'
#'   \item{[add_random_solver()]}{Generate solutions by
#'     randomly funding actions. This can be useful when evaluating the
#'     performance of a funding scheme---though it is strongly recommended
#'     to evaluate the performance of a funding scheme by comparing it
#'     to an optimal solution identified using exact algorithms (e.g.
#'     [add_gurobi_solver()], [add_rsymphony_solver()]).}
#'
#' }
#'
#' @name solvers
#'
#' @seealso [constraints],  [decisions],
#'  [objectives], [problem()], [targets].
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem
#' p1 <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions()
#'
#' # build another problem, with the default solver
#' p2 <- p1 %>%
#'       add_default_solver()
#'
#' # build another problem, with the gurobi solver
#' \dontrun{
#' p3 <- p1 %>%
#'       add_gurobi_solver()
#' }
#'
#' # build another problem, with the Rsympony solver
#' \dontrun{
#' p4 <- p1 %>%
#'       add_rsymphony_solver()
#' }
#'
#' # build another problem, with the lpsymphony solver
#' \dontrun{
#' p5 <- p1 %>%
#'       add_lpsymphony_solver()
#' }
#'
#' # build another problem, with the lpSolveAPI solver
#' p6 <- p1 %>%
#'       add_lpsolveapi_solver()
#'
#' # build another problem, with the heuristic solver
#' p7 <- p1 %>%
#'       add_heuristic_solver()
#'
#' # build another problem, with the random solver
#' p8 <- p1 %>%
#'       add_random_solver()
#'
#' \dontrun{
#' # generate solutions using each of the solvers
#' s <- rbind(solve(p2), solve(p3), solve(p4), solve(p5), solve(p6), solve(p7),
#'            solve(p8))
#' s$solver <- c("default", "gurobi", "Rsymphony", "lpsymphony", "lpSolveAPI",
#'               "heuristic", "random")
#'
#' # print solutions
#' print(as.data.frame(s))
#' }
NULL
