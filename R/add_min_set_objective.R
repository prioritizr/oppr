#' @include internal.R Objective-class.R star_phylogeny.R
NULL

#' Add minimum set objective
#'
#' Add an objective to a project prioritization problem based on
#' minimizing the cost of the solution, whilst ensuring that the target
#' thresholds for each feature is met (Chadés *et al.* 2015).
#' In some project prioritization exercises,
#' decision makers may have a target threshold level of expected outcome
#' for each feature (e.g., a 90% chance of persistence for each feature).
#' This objective is especially useful for identifying solutions
#' that ensure that each feature achieves, at least, some minimum level
#' of expected outcome based on the target thresholds.
#'
#' @inheritParams add_max_wtd_sum_objective
#'
#' @details
#' A problem objective is used to specify the overall goal of the
#' project prioritization problem.
#' Here, the minimum set objective seeks to find the set of actions that
#' minimizes the overall cost of the prioritization, while ensuring that the
#' funded projects meet a set of targets for the expected outcomes
#' (e.g., probabilities of persistence, population sizes, amount of habitat)
#' associated with conservation features (e.g., populations, species,
#' ecosystems). Let \eqn{I} represent
#' the set of conservation actions (indexed by \eqn{i}). Let \eqn{C_i} denote
#' the cost for funding action \eqn{i}. Also, let \eqn{F} represent each
#' feature (indexed by \eqn{f}), \eqn{T_f} represent the target
#' for feature \eqn{f}, and \eqn{E_f} denote the expected outcome for each
#' feature given the funded conservation projects.
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
#' variables in this problem are the \eqn{Y_{j}}, \eqn{Z_{fj}}, and \eqn{E_f}
#' variables. Specifically, the binary \eqn{Y_{j}} variables indicate if
#' project \eqn{j} is funded or not based on which actions are funded; the
#' binary \eqn{Z_{fj}} variables indicate if project \eqn{j} is used to manage
#' feature \eqn{f} or not; and the continuous \eqn{E_f} variables
#' denote the expected outcome for feature \eqn{f}.
#'
#' Now that we have defined all the data and variables, we can formulate
#' the problem. For convenience, let the symbol used to denote each set also
#' represent its cardinality (e.g., if there are ten features, let \eqn{F}
#' represent the set of ten features and also the number ten).
#'
#' \deqn{
#'   \mathrm{Minimize} \space \sum_{i = 0}^{I} C_i X_i \space
#'   \mathrm{(eqn \space 1a)} \\
#'   \mathrm{Subject \space to} \space  \\
#'   E_f \geq T_f \space \forall f \in F \space
#'   \mathrm{(eqn \space 1b)} \\
#'   E_f = \sum_{j = 0}^{J} Z_{fj} Q_{fj} \space \forall \space f \in F
#'   \space \mathrm{(eqn \space 1c)} \\
#'   Z_{fj} \leq Y_{j} \space \forall \space j \in J \space \mathrm{(eqn \space
#'   1d)} \\
#'   \sum_{j = 0}^{J} Z_{fj} \times \mathrm{ceil}(Q_{fj}) = 1 \space \forall
#'   \space f \in F \space \mathrm{(eqn \space 1e)} \\
#'   A_{ij} Y_{j} \leq X_{i} \space \forall \space i \in I, j \in J \space
#'   \mathrm{(eqn \space 1f)} \\
#'   E_{f} \geq 0 \space \forall \space f \in F \space
#'   \mathrm{(eqn \space 1g)} \\
#'   X_{i}, Y_{j}, Z_{fj} \in \{0, 1\} \space \forall \space i \in I, j \in J, f
#'   \in F \space \mathrm{(eqn \space 1h)}
#' }{
#'   Maximize sum_i^I C_i X_i (eqn 1a);
#'   Subject to:
#'   E_f <= T_f for all f in F (eqn 1b),
#'   E_f = sum_j^J Y_{fj} Q_{fj} for all f in F (eqn 1c),
#'   Z_{fj} <= Y_j for all j in J (eqn 1d),
#'   sum_j^J Z_{fj} * ceil(Q_{fj}) = 1 for all f in F (eqn 1e),
#'   A_{ij} Y_{j} <= X_{i} for all i I, j in J (eqn 1f),
#'   E_f >= 0 for all f in F (eqn 1g),
#'   X_i, Y_j, Z_{fj} in \{0, 1\} for all i in I, j in J, f in F (eqn 1h)
#' }
#'
#' The objective (eqn 1a) is to minimize the cost of the funded actions.
#' Constraints (eqn 1b) ensure that the targets are met.
#' Constraints (eqn 1c) calculate the expected outcome for each feature
#' according to their allocated project.
#' Constraints (eqn 1d) ensure that feature can only be allocated to projects
#' that have all of their actions funded. Constraints (eqn 1e) state that each
#' feature can only be allocated to a single project. Constraints (eqn 1f)
#' ensure that a project cannot be funded unless all of its actions are
#' funded. Constraints (eqns 1g) ensure that the expected outcome variables
#' (\eqn{E_f}) are greater than zero. Constraints (eqns 1h) ensure
#' that the action funding (\eqn{X_i}), project funding (\eqn{Y_j}), and
#' project allocation (\eqn{Z_{fj}}) variables are binary.
#'
#' @references
#' Chadés I, Nicol S, van Leeuwen S, Walters B, Firn J, Reeson A, Martin TG &
#' Carwardine J (2015) Benefits of integrating complementarity into priority
#' threat management. Conservation Biology **29**: 525--536.
#'
#' @family objectives
#'
#' @inherit add_max_wtd_sum_objective return seealso
#'
#' @seealso
#' See [targets] for an overview of functions for adding targets.
#'
#' @examplesIf oppr::run_example()
#' # load the ggplot2 R package to customize plot
#' library(ggplot2)
#'
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with minimum set objective and targets that require each
#' # feature to have a 30% chance of persisting into the future
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(0.3) %>%
#'   add_binary_decisions()
#'
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#'
#' # plot solution, and add a dashed line to indicate the feature targets
#' plot(p, s) + geom_hline(yintercept = 0.3, linetype = "dashed")
#' @name add_min_set_objective
NULL

#' @rdname add_min_set_objective
#' @export
add_min_set_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ProjectProblem"))
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "MinimumSetObjective",
      inherit = Objective,
      public = list(
        name = "minimum set objective",
        has_targets = TRUE,
        has_weights = FALSE,
        data = list(feature_names = feature_names(x)),
        feature_phylogeny = function() {
          star_phylogeny(self$data$feature_names)
        },
        default_feature_weights = function() {
          stats::setNames(
            rep(NA_real_, length(self$data$feature_names)),
            self$data$feature_names
          )
        },
        evaluate = function(y, solution) {
          assertthat::assert_that(
            inherits(y, "ProjectProblem"),
            inherits(solution, "tbl_df")
          )
          fp <- y$feature_phylogeny()
          bm <- branch_matrix(fp, FALSE)
          bo <- rcpp_branch_order(bm)
          rcpp_evaluate_min_set_objective(
            y$action_costs(),
            y$pa_matrix(),
            y$eof_matrix()[, y$feature_phylogeny()$tip.label, drop = FALSE],
            bm[, bo, drop = FALSE],
            fp$edge.length[bo],
            rep(0, y$number_of_features()),
            rep(0, y$number_of_features()),
            as_Matrix(as.matrix(solution), "dgCMatrix")
          )
        },
        apply = function(x, y) {
          assertthat::assert_that(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ProjectProblem")
          )
          invisible(
            rcpp_apply_min_set_objective(
              x$ptr, y$feature_targets(), y$action_costs()
            )
          )
        }
      )
    )$new()
  )
}
