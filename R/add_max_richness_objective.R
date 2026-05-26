#' @include internal.R Objective-class.R star_phylogeny.R
NULL

#' Add maximum richness objective
#'
#' Add an objective to a project prioritization problem based on
#' maximizing the number of features likely to persist, whilst ensuring that
#' the  cost of the solution is within a pre-specified budget
#' (Joseph, Maloney & Possingham 2009).
#' Weights can be used to specify the relative importance of conserving specific
#' features (see [add_feature_weights()]).
#' Although this objective is conceptually
#' similar to maximizing the weighted sum of expected outcomes for the
#' features ([add_max_richness_objective()]
#' it is designed to work for features that are associated with
#' probabilistic outcomes.
#'
#' @inheritParams add_max_wtd_sum_objective
#'
#' @details
#' A problem objective is used to specify the overall goal of the
#' project prioritization problem.
#' Here, the maximum richness objective seeks to find the set of actions that
#' maximizes the total number of features  (e.g., populations, species,
#' ecosystems) that are expected to persist within a pre-specified budget.
#' Let \eqn{I} represent the set of conservation actions (indexed by
#' \eqn{i}). Let \eqn{C_i} denote the cost for funding action \eqn{i}, and
#' let \eqn{m} denote the maximum expenditure (i.e., the budget). Also,
#' let \eqn{F} represent each feature (indexed by \eqn{f}), \eqn{W_f}
#' represent the weight for each feature \eqn{f} (defaults to one for
#' each feature unless specified otherwise), and \eqn{E_f} denote the
#' probability that each feature will go extinct given the funded
#' conservation projects.
#'
#' To guide the prioritization, the conservation actions are organized into
#' conservation projects. Let \eqn{J} denote the set of conservation projects
#' (indexed by \eqn{j}), and let \eqn{A_{ij}} denote which actions
#' \eqn{i \in I}{i in I} comprise each conservation project
#' \eqn{j \in J}{j in J} using zeros and ones. Next, let \eqn{P_j} represent
#' the probability of project \eqn{j} being successful if it is funded. Also,
#' let \eqn{B_{fj}} denote the probability that each feature
#' \eqn{f \in F}{f in F} associated with the project \eqn{j \in J}{j in J}
#' will persist if all of the actions that comprise project \eqn{j} are funded
#' and that project is allocated to feature \eqn{f}. For convenience,
#' let \eqn{Q_{fj}} denote the actual probability that each
#' \eqn{f \in F}{f in F} associated with the project \eqn{j \in J}{j in J}
#' is expected to persist if the project is funded. If the argument
#' to `adjust_for_baseline` in the `problem` function was set to
#' `TRUE`, and this is the default behavior, then
#' \eqn{Q_{fj} = (P_{j} \times B_{fj}) + \bigg(\big(1 - (P_{j} B_{fj})\big)
#' \times (P_{n} \times B_{fn})\bigg)}{Q_{fj} = (P_j B_{fj}) + ((1 - (P_j
#' B_{fj})) * (P_n \times B_{fn}))}, where `n` corresponds to the
#' baseline "do nothing" project. This means that the probability
#' of a feature persisting if a project is allocated to a feature
#' depends on (i) the probability of the project succeeding, (ii) the
#' probability of the feature persisting if the project does not fail,
#' and (iii) the probability of the feature persisting even if the project
#' fails. Otherwise, if the argument is set to `FALSE`, then
#' \eqn{Q_{fj} = P_{j} \times B_{fj}}{Q_{fj} = P_{j} * B_{fj}}.
#'
#' The binary control variables \eqn{X_i} in this problem indicate whether
#' each project \eqn{i \in I}{i in I} is funded or not. The decision
#' variables in this problem are the \eqn{Y_{j}}, \eqn{Z_{fj}}, and \eqn{E_f}
#' variables.
#' Specifically, the binary \eqn{Y_{j}} variables indicate if project \eqn{j}
#' is funded or not based on which actions are funded; the binary
#' \eqn{Z_{fj}} variables indicate if project \eqn{j} is used to manage
#' feature \eqn{f} or not; and the continuous \eqn{E_f} variables
#' denote the probability that feature \eqn{f} will persist.
#'
#' Now that we have defined all the data and variables, we can formulate
#' the problem. For convenience, let the symbol used to denote each set also
#' represent its cardinality (e.g., if there are ten features, let \eqn{F}
#' represent the set of ten features and also the number ten).
#'
#' \deqn{
#'   \mathrm{Maximize} \space \sum_{f = 0}^{F} E_f W_f \space
#'   \mathrm{(eqn \space 1a)} \\
#'   \mathrm{Subject \space to} \sum_{i = 0}^{I} C_i \leq m \space
#'   \mathrm{(eqn \space 1b)} \\
#'   E_f = \sum_{j = 0}^{J} Z_{fj} Q_{fj} \space \forall \space f \in F
#'   \space \mathrm{(eqn \space 1c)} \\
#'   Z_{fj} \leq Y_{j} \space \forall \space j \in J \space \mathrm{(eqn \space
#'   1d)} \\
#'   \sum_{j = 0}^{J} Z_{fj} \times \mathrm{ceil}(Q_{fj}) = 1 \space \forall
#'   \space f \in F \space \mathrm{(eqn \space 1e)} \\
#'   A_{ij} Y_{j} \leq X_{i} \space \forall \space i \in I, j \in J \space
#'   \mathrm{(eqn \space 1f)} \\
#'   E_{f} \geq 0, E_{f} \leq 1 \space \forall \space f \in F \space
#'   \mathrm{(eqn \space 1g)} \\
#'   X_{i}, Y_{j}, Z_{fj} \in \{0, 1\} \space \forall \space i \in I, j \in J, f
#'   \in F \space \mathrm{(eqn \space 1h)}
#' }{
#'   Maximize sum_f^F E_f W_f (eqn 1a);
#'   Subject to:
#'   sum_i^I C_i X_i <= m for all f in F (eqn 1b),
#'   E_f = sum_j^J Y_{fj} Q_{fj} for all f in F (eqn 1c),
#'   Z_{fj} <= Y_j for all j in J (eqn 1d),
#'   sum_j^J Z_{fj} * ceil(Q_{fj}) = 1 for all f in F (eqn 1e),
#'   A_{ij} Y_{j} <= X_{i} for all i I, j in J (eqn 1f),
#'   E_f >= 0, E_f <= 1 for all f in F (eqn 1g),
#'   X_i, Y_j, Z_{fj} in \{0, 1\f] for all i in I, j in J, f in F (eqn 1h)
#' }
#'
#' The objective (eqn 1a) is to maximize the weighted number of features
#' that are expected to persist. Constraint (eqn 1b) limits the maximum
#' expenditure (i.e., ensures
#' that the cost of the funded actions do not exceed the budget).
#' Constraints (eqn 1c) calculate the probability that each feature
#' will go extinct according to their allocated project.
#' Constraints (eqn 1d) ensure that feature can only be allocated to projects
#' that have all of their actions funded. Constraints (eqn 1e) state that each
#' feature can only be allocated to a single project. Constraints (eqn 1f)
#' ensure that a project cannot be funded unless all of its actions are funded.
#' Constraints (eqns 1g) ensure that the probability variables
#' (\eqn{E_f}) are bounded between zero and one. Constraints (eqns 1h) ensure
#' that the action funding (\eqn{X_i}), project funding (\eqn{Y_j}), and project
#' allocation (\eqn{Z_{fj}}) variables are binary.
#'
#' @references
#' Joseph LN, Maloney RF & Possingham HP (2009) Optimal allocation of
#' resources among threatened species: A project prioritization protocol.
#' *Conservation Biology*, **23**, 328--338.
#'
#' @inherit add_max_wtd_sum_objective seealso return
#'
#' @family objectives
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with maximum richness objective and $300 budget
#' p1 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_richness_objective(budget = 200) %>%
#'   add_binary_decisions()
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(p1, s1)
#'
#' # build another problem that includes feature weights
#' p2 <- p1 %>% add_feature_weights("weight")
#'
#' # solve problem with feature weights
#' s2 <- solve(p2)
#'
#' # print solution based on feature weights
#' print(s2)
#'
#' # plot solution based on feature weights
#' plot(p2, s2)
#' @name add_max_richness_objective
NULL

#' @rdname add_max_richness_objective
#' @export
add_max_richness_objective <- function(x, budget) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, "ProjectProblem"),
    assertthat::is.number(budget),
    assertthat::noNA(budget),
    isTRUE(budget >= 0)
  )
  assertthat::assert_that(
    all(x$of_matrix() >= 0, na.rm = TRUE),
    all(x$of_matrix() <= 1, na.rm = TRUE),
    msg = paste(
      "The outcome associated with each project in `x`",
      "must be a probability value for all features."
    )
  )
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "MaxWeightedSumObjective",
      inherit = Objective,
      public = list(
        name = "maximum richness objective",
        has_targets = FALSE,
        has_weights = TRUE,
        data = list(feature_names = x$feature_names(), budget = budget),
        feature_phylogeny = function() {
          star_phylogeny(
            self$data$feature_names,
            rep(0, length(self$data$feature_names))
          )
        },
        default_feature_weights = function() {
          stats::setNames(
            rep(1, length(self$data$feature_names)),
            self$data$feature_names
          )
        },
        replace_feature_weights = function() {
          TRUE
        },
        evaluate = function(y, solution) {
          assertthat::assert_that(
            inherits(y, "ProjectProblem"),
            inherits(solution, "tbl_df")
          )
          fp <- y$feature_phylogeny()
          bm <- branch_matrix(fp, FALSE)
          bo <- rcpp_branch_order(bm)
          w <- y$feature_weights()[y$feature_phylogeny()$tip.label]
          rcpp_evaluate_max_phylo_div_objective(
            y$action_costs(),
            y$pa_matrix(),
            y$eof_matrix()[, y$feature_phylogeny()$tip.label, drop = FALSE],
            bm[, bo, drop = FALSE],
            rep(0, ncol(bm)),
            rep(0, y$number_of_features()),
            w,
            as_Matrix(as.matrix(solution), "dgCMatrix")
          )
        },
        apply = function(x, y) {
          assertthat::assert_that(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ProjectProblem")
          )
          fp <- y$feature_phylogeny()
          bo <- rcpp_branch_order(branch_matrix(fp, FALSE))
          invisible(
            rcpp_apply_max_phylo_div_objective(
              x$ptr,
              y$action_costs(),
              self$get_data("budget"),
              fp$edge.length[bo],
              rep(1, y$number_of_features())
            )
          )
        }
      )
    )$new()
  )
}
