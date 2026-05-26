#' @include internal.R Objective-class.R star_phylogeny.R
NULL

#' Add maximum targets met objective
#'
#' Add an objective to a project prioritization problem based on
#' maximizing the number of targets met, whilst ensuring that the cost of the
#' solution is within a pre-specified budget
#' (Chades *et al.* 2015). In some project prioritization exercises,
#' decision makers may have a target threshold level of expected outcome
#' for each feature (e.g., a 90% chance of persistence for each feature).
#' In such exercises, the decision makers
#' do not perceive any benefit when a target is not met (e.g., if a feature
#' has a target corresponding to a 90% chance of persistence, then no benefit
#' is accrued if the feature has a 50% chance of persistence), and do not
#' assign any greater benefit for surpassing a target (e.g., if a feature has a
#' target corresponding to a 90% chance of persistence, then the same level
#' of benefit would be accrued if the feature had a 90% or 95% chance
#' of persistence). Furthermore, weights can also be used to specify the
#' relative importance of meeting targets for particular features
#' (see [add_feature_weights()]).
#'
#' @inheritParams add_max_wtd_sum_objective
#'
#' @details
#' A problem objective is used to specify the overall goal of the
#' project prioritization problem.
#' Here, the maximum targets met objective seeks to find the set of actions
#' that maximizes the total number of features (e.g., populations, species,
#' ecosystems) that have met their targets within a
#' pre-specified budget. Let \eqn{I} represent the set of conservation
#' actions (indexed by \eqn{i}). Let \eqn{C_i} denote the cost for funding
#' action \eqn{i}, and let \eqn{m} denote the maximum expenditure (i.e., the
#' budget). Also, let \eqn{F} represent each feature (indexed by \eqn{f}),
#' \eqn{W_f} represent the weight for each feature \eqn{f} (defaults to one
#' for each feature unless specified otherwise), \eqn{T_f} represent the
#' target for each feature \eqn{f}, and \eqn{E_f} denote the
#' expected outcome for each feature given the funded conservation projects.
#'
#' To guide the prioritization, the conservation actions are organized into
#' conservation projects. Let \eqn{J} denote the set of conservation projects
#' (indexed by \eqn{j}), and let \eqn{A_{ij}} denote which actions
#' \eqn{i \in I}{i in I} comprise each conservation project
#' \eqn{j \in J}{j in J} using zeros and ones. Next, let \eqn{P_j} represent
#' the probability of project \eqn{j} being successful if it is funded. Also,
#' let \eqn{B_{fj}} denote the outcome for each feature
#' \eqn{f \in F}{f in F} associated with the project \eqn{j \in J}{j in J}
#' assuming that all of the actions comprising project \eqn{j} are funded
#' and that project is allocated to feature \eqn{f}. For convenience,
#' let \eqn{Q_{fj}} denote the expected outcome for each
#' \eqn{f \in F}{f in F} associated with the project \eqn{j \in J}{j in J}
#' if the project is funded. If the argument
#' to `adjust_for_baseline` in the `problem` function was set to
#' `TRUE`, and this is the default behavior, then
#' \eqn{Q_{fj} = (P_{j} \times B_{fj}) + \bigg(\big(1 - (P_{j} B_{fj})\big)
#' \times (P_{n} \times B_{fn})\bigg)}{Q_{fj} = (P_j B_{fj}) + ((1 - (P_j
#' B_{fj})) * (P_n \times B_{fn}))}, where `n` corresponds to the
#' baseline "do nothing" project. This means that the expected outcome
#' for a feature given that a project is funded and allocated to the feature
#' depends on (i) the probability of the project succeeding, (ii) the
#' outcome for the feature if the project succeeds,
#' and (iii) the outcome for the feature if the project fails
#' (per the baseline project).
#' Otherwise, if the argument is set to `FALSE`, then
#' \eqn{Q_{fj} = P_{j} \times B_{fj}}{Q_{fj} = P_{j} * B_{fj}}.
#'
#' The binary control variables \eqn{X_i} in this problem indicate whether
#' each project \eqn{i \in I}{i in I} is funded or not. The decision
#' variables in this problem are the \eqn{Y_{j}}, \eqn{Z_{fj}}, \eqn{E_f},
#' and \eqn{G_f} variables.
#' Specifically, the binary \eqn{Y_{j}} variables indicate if project \eqn{j}
#' is funded or not based on which actions are funded; the binary
#' \eqn{Z_{fj}} variables indicate if project \eqn{j} is used to manage
#' feature \eqn{f} or not; the continuous \eqn{E_f} variables
#' denote the expected outcome for feature \eqn{f}; and the binary
#' \eqn{G_f} variables indicate if the target for feature
#' \eqn{f} is met.
#'
#' Now that we have defined all the data and variables, we can formulate
#' the problem. For convenience, let the symbol used to denote each set also
#' represent its cardinality (e.g., if there are ten features, let \eqn{F}
#' represent the set of ten features and also the number ten).
#'
#' \deqn{
#'   \mathrm{Maximize} \space \sum_{f = 0}^{F} G_f W_f \space
#'   \mathrm{(eqn \space 1a)} \\
#'   \mathrm{Subject \space to}
#'   \sum_{i = 0}^{I} C_i \leq m \space \mathrm{(eqn \space 1b)} \\
#'   E_f \geq G_f T_f \space \forall \space f \in F \space
#'   \mathrm{(eqn \space 1c)} \\
#'   E_f = \sum_{j = 0}^{J} Z_{fj} Q_{fj} \space \forall \space f \in F
#'   \space \mathrm{(eqn \space 1d)} \\
#'   Z_{fj} \leq Y_{j} \space \forall \space j \in J \space \mathrm{(eqn \space
#'   1e)} \\
#'   \sum_{j = 0}^{J} Z_{fj} \times \mathrm{ceil}(Q_{fj}) = 1 \space \forall
#'   \space f \in F \space \mathrm{(eqn \space 1f)} \\
#'   A_{ij} Y_{j} \leq X_{i} \space \forall \space i \in I, j \in J \space
#'   \mathrm{(eqn \space 1g)} \\
#'   E_{f} \geq 0 \space \forall \space f \in F \space
#'   \mathrm{(eqn \space 1h)} \\
#'   G_{f}, X_{i}, Y_{j}, Z_{fj} \in \{0, 1\} \space \forall \space i \in I, j
#'   \in J, f \in F \space \mathrm{(eqn \space 1i)}
#' }{
#'   Maximize sum_f^F G_f W_f (eqn 1a);
#'   Subject to:
#'   sum_i^I C_i X_i <= m for all f in F (eqn 1b),
#'   E_f >= G_f T_f for all f \in F (eqn 1c),
#'   E_f = sum_j^J Y_{fj} Q_{fj} for all f in F (eqn 1d),
#'   Z_{fj} <= Y_j for all j in J (eqn 1e),
#'   sum_j^J Z_{fj} * ceil(Q_{fj}) = 1 for all f in F (eqn 1f),
#'   A_{ij} Y_{j} <= X_{i} for all i I, j in J (eqn 1g),
#'   E_f >= 0 for all f in F (eqn 1h),
#'   G_f, X_i, Y_j, Z_{fj} in \{0, 1\} for all i in I, j in J, f in F (eqn 1i)
#' }
#'
#' The objective (eqn 1a) is to maximize the weighted total number of the
#' features that have their targets met.
#' Constraints (eqn 1b) calculate which targets have been met.
#' Constraint (eqn 1c) limits the maximum expenditure (i.e., ensures
#' that the cost of the funded actions do not exceed the budget).
#' Constraints (eqn 1d) calculate the expected outcome for each feature
#' according to their allocated project.
#' Constraints (eqn 1e) ensure that feature can only be allocated to projects
#' that have all of their actions funded. Constraints (eqn 1f) state that each
#' feature can only be allocated to a single project. Constraints (eqn 1g)
#' ensure that a project cannot be funded unless all of its actions are funded.
#' Constraints (eqns 1h) ensure that the expected outcome variables
#' (\eqn{E_f}) are greater than zero. Constraints (eqns 1i) ensure
#' that the target met (\eqn{G_f}), action funding (\eqn{X_i}), project funding
#' (\eqn{Y_j}), and project allocation (\eqn{Z_{fj}}) variables are binary.
#'
#' @references
#' Chades I, Nicol S, van Leeuwen S, Walters B, Firn J, Reeson A, Martin TG &
#' Carwardine J (2015) Benefits of integrating complementarity into priority
#' threat management. *Conservation Biology*, **29**, 525--536.
#'
#' @inherit add_min_set_objective seealso return
#'
#' @family objectives
#'
#' @examplesIf oppr::run_example()
#' # load the ggplot2 R package to customize plot
#' library(ggplot2)
#'
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # manually adjust feature weights
#' sim_features$weight <- c(8, 2, 6, 3, 1)
#'
#' # build problem with maximum targets met objective, a $200 budget,
#' # targets that require each feature to have a 20% chance of persisting into
#' # the future, and zero cost actions locked in
#' p1 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_targets_met_objective(budget = 200) %>%
#'   add_absolute_targets(0.2) %>%
#'   add_locked_in_action_constraints(which(sim_actions$cost < 1e-5)) %>%
#'   add_binary_decisions()
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution, and add a dashed line to indicate the feature targets
#' # we can see the three features meet the targets under the baseline
#' # scenario, and the project for F5 was prioritized for funding
#' # so that its probability of persistence meets the target
#' plot(p1, s1) + geom_hline(yintercept = 0.2, linetype = "dashed")
#'
#' # build another problem that includes feature weights
#' p2 <- p1 %>% add_feature_weights("weight")
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # plot solution, and add a dashed line to indicate the feature targets
#' # we can see that adding weights to the problem has changed the solution
#' # specifically, the projects for the feature F3 is now funded
#' # to enhance its probability of persistence
#' plot(p2, s2) + geom_hline(yintercept = 0.2, linetype = "dashed")
#' @name add_max_targets_met_objective
NULL

#' @rdname add_max_targets_met_objective
#' @export
add_max_targets_met_objective <- function(x, budget) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, "ProjectProblem"),
    assertthat::is.number(budget),
    assertthat::noNA(budget),
    isTRUE(budget >= 0)
  )
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "MaximumTargetsMetObjective",
      inherit = Objective,
      public = list(
        name = "maximum targets met objective",
        has_targets = TRUE,
        has_weights = TRUE,
        data = list(feature_names = x$feature_names(), budget = budget),
        feature_phylogeny = function() {
          star_phylogeny(self$data$feature_names)
        },
        replace_feature_weights = function() {
          TRUE
        },
        default_feature_weights = function() {
          stats::setNames(
            rep(1, length(self$data$feature_names)),
            self$data$feature_names
          )
        },
        evaluate = function(y, solution) {
          assertthat::assert_that(
            inherits(y, "ProjectProblem"),
            inherits(solution, "tbl_df")
          )
          if (is.Waiver(y$targets)) {
            y <- add_default_targets(y)
          }
          fp <- y$feature_phylogeny()
          bm <- branch_matrix(fp, FALSE)
          bo <- rcpp_branch_order(bm)
          w <- y$feature_weights()[y$feature_phylogeny()$tip.label]
          rcpp_evaluate_max_targets_met_objective(
            y$action_costs(),
            y$pa_matrix(),
            y$eof_matrix(),
            bm[, bo, drop = FALSE], fp$edge.length[bo],
            y$targets$output()$value,
            w,
            as_Matrix(as.matrix(solution), "dgCMatrix")
          )
        },
        apply = function(x, y) {
          assertthat::assert_that(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ProjectProblem")
          )
          invisible(
            rcpp_apply_max_targets_met_objective(
              x$ptr,
              y$feature_targets(),
              y$action_costs(),
              self$get_data("budget"),
              rep(1, y$number_of_features())
            )
          )
        }
      )
    )$new()
  )
}
