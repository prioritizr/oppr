#' @include internal.R pproto.R Objective-proto.R
NULL

#' Problem objective
#'
#' An objective is used to specify the overall goal of a project prioritization
#' [problem()]. All project prioritization problems involve
#' minimizing or maximizing some kind of objective. For instance, the decision
#' maker may require a funding scheme that maximizes the total number of
#' species that are expected to persist into the future whilst ensuring that
#' the total cost of the funded actions does not exceed a budget.
#' Alternatively, the planner may require a solution that ensures that
#' each species meets a target level of persistence whilst minimizing the cost
#' of  the funded actions. A project prioritization
#' [problem()] **must** have a specified objective before it can
#' be solved, and attempting to solve a problem which does not have
#' a specified objective will throw an error.
#'
#' @details The following objectives can be added to a conservation planning
#'   [problem()]:
#'
#'   \describe{
#'
#'   \item{[add_max_richness_objective()]}{
#'     Maximize the total number of features that are expected to persist,
#'     whilst ensuring that the cost of the solution is within a pre-specified
#'     budget (Joseph, Maloney & Possingham 2009).}
#'
#'   \item{[add_max_targets_met_objective()]}{
#'     Maximize the total number of persistence targets met for the features,
#'     whilst ensuring that the cost of the solution is within a pre-specified
#'     budget (Chades *et al.* 2015).}
#'
#'   \item{[add_max_phylo_div_objective()]}{
#'     Maximize the phylogenetic diversity that is expected to persist into the
#'     future, whilst ensuring that the cost of the solution is within a
#'     pre-specified budget (Bennett *et al.* 2014, Faith 2008).}
#'
#'   \item{[add_min_set_objective()]}{
#'     Minimize the cost of the solution whilst ensuring that all targets are
#'     met. This objective is conceptually similar to that used in *Marxan*
#'     (Ball, Possingham & Watts 2009).}
#'
#'   }
#'
#' @references
#' Ball IR, Possingham HP & Watts M (2009) Marxan and relatives: software for
#' spatial conservation prioritisation.
#' *Spatial conservation prioritisation: Quantitative methods and
#' computational tools*, 185-195.
#'
#' Bennett JR, Elliott G, Mellish B, Joseph LN, Tulloch AI,
#' Probert WJ, Di Fonzo MMI, Monks JM, Possingham HP & Maloney R (2014)
#' Balancing phylogenetic diversity
#' and species numbers in conservation prioritization, using a case study of
#' threatened species in New Zealand. *Biological Conservation*,
#' **174**: 47--54.
#'
#' Chades I, Nicol S, van Leeuwen S, Walters B, Firn J, Reeson A, Martin TG &
#' Carwardine J (2015) Benefits of integrating complementarity into priority
#' threat management. *Conservation Biology*, **29**, 525--536.
#'
#' Faith DP (2008) Threatened species and the potential loss of
#' phylogenetic diversity: conservation scenarios based on estimated extinction
#' probabilities and phylogenetic risk analysis. *Conservation Biology*,
#' **22**: 1461--1470.
#'
#' Joseph LN, Maloney RF & Possingham HP (2009) Optimal allocation of
#' resources among threatened species: A project prioritization protocol.
#' *Conservation Biology*, **23**, 328--338.
#'
#' @seealso [constraints], [decisions],
#'   [problem()], [solvers], [targets],
#'   [weights].
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions, sim_tree)
#'
#' # build problem with maximum richness objective and $200 budget
#' p1 <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions()
#'
#' \dontrun{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(p1, s1)
#' }
#'
#' # build problem with maximum phylogenetic diversity objective and $200 budget
#' p2 <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_phylo_div_objective(budget = 200, tree = sim_tree) %>%
#'      add_binary_decisions()
#'
#' \dontrun{
#' # solve problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' plot(p2, s2)
#' }
#' # build problem with maximum targets met objective, $200 budget, and
#' # 40% persistence targets
#' p3 <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_targets_met_objective(budget = 200) %>%
#'      add_absolute_targets(0.4) %>%
#'      add_binary_decisions()
#'
#' \dontrun{
#' # solve problem
#' s3 <- solve(p3)
#'
#' # print solution
#' print(s3)
#'
#' # plot solution
#' plot(p3, s3)
#' }
#'
#' # build problem with minimum set objective, $200 budget, and 40%
#' # persistence targets
#' p4 <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_min_set_objective() %>%
#'      add_absolute_targets(0.4) %>%
#'      add_binary_decisions()
#'
#' \dontrun{
#' # solve problem
#' s4 <- solve(p4)
#'
#' # print solution
#' print(s4)
#'
#' # plot solution
#' plot(p4, s4)
#' }
#' @name objectives
NULL

add_default_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ProjectProblem"))
  # throw error because objectives must be explicitly defined
  stop("problem is missing an objective and this must be explicitly defined")
}
