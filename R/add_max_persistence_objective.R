#' @include internal.R pproto.R Objective-proto.R star_phylogeny.R
NULL

#' Add maximum persistence objective
#'
#' Set the objective of a project prioritization \code{\link{problem}} to
#' maximize the chance that at least one feature will persist into the future,
#' whilst ensuring that the cost of the solution is within a pre-specified
#' budget. This objective is conceptually similar to maximizing feature
#' richness (i.e. \code{\link{add_max_richness_objective}}), except that this
#' objective tends to favor solutions which spread resources across
#' many features---instead of triaging features---because each feature is
#' treated as an independent "backup". Furthermore, weights can
#' also be used to specify the relative importance of conserving specific
#' features (see \code{\link{add_feature_weights}}).
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param budget \code{numeric} budget for funding actions.
#'
#' @details A problem objective is used to specify the overall goal of the
#'   project prioritization problem.
#'   Here, the maximum persistence objective seeks to find the set of actions
#'   that maximizes the chance that at least a single feature (e.g.
#'   populations, species, eco-systems) will persist into the future, whilst
#'   ensuring that the total cost of the actions remains within a pre-specified
#'   budget. Let \eqn{I} represent the set of conservation actions (indexed by
#'   \eqn{i}). Let \eqn{C_i} denote the cost for funding action \eqn{i}, and
#'   let \eqn{m} denote the maximum expenditure (i.e. the budget). Also,
#'   let \eqn{F} represent each feature (indexed by \eqn{f}), \eqn{W_f}
#'   represent the weight for each feature \eqn{f} (defaults to zero for
#'   each feature unless specified otherwise), and \eqn{E_f}
#'   denote the probability that each feature will go extinct given the funded
#'   conservation projects.
#'
#'   To guide the prioritization, the conservation actions are organized into
#'   conservation projects. Let \eqn{J} denote the set of conservation projects
#'   (indexed by \eqn{j}), and let \eqn{A_{ij}} denote which actions
#'   \eqn{i \in I}{i in I} comprise each conservation project
#'   \eqn{j \in J}{j in J} using zeros and ones. Next, let \eqn{P_j} represent
#'   the probability of project \eqn{j} being successful if it is funded. Also,
#'   let \eqn{B_{fj}} denote the enhanced probability that each feature
#'   \eqn{f \in F}{f in F} associated with the project \eqn{j \in J}{j in J}
#'   will persist if all of the actions that comprise project \eqn{j} are funded
#'   and that project is allocated to feature \eqn{f}.
#'
#'   The binary control variables \eqn{X_i} in this problem indicate whether
#'   each project \eqn{i \in I}{i in I} is funded or not. The decision
#'   variables in this problem are the \eqn{Y_{j}}, \eqn{Z_{fj}}, and \eqn{E_f}
#'   variables.
#'   Specifically, the binary \eqn{Y_{j}} variables indicate if project \eqn{j}
#'   is funded or not based on which actions are funded; the binary
#'   \eqn{Z_{fj}} variables indicate if project \eqn{j} is used to manage
#'   feature \eqn{f} or not; and the semi-continuous \eqn{E_f} variables
#'   denote the probability that feature \eqn{f} will go extinct.
#'
#'   Now that we have defined all the data and variables, we can formulate
#'   the problem. For convenience, let the symbol used to denote each set also
#'   represent its cardinality (e.g. if there are ten features, let \eqn{F}
#'   represent the set of ten features and also the number ten).
#'
#' \deqn{
#'   \mathrm{Maximize} (1 - \prod_{f = 0}^{F} E_f W_f) + \sum_{f}^{F}
#'   (1 - E_f) W_f \space \mathrm{(eqn \space 1a)} \\
#'   \mathrm{Subject \space to}
#'   \sum_{i = 0}^{I} C_i \leq m \space \mathrm{(eqn \space 1b)} \\
#'   E_f = 1 - \sum_{j = 0}^{J} Z_{fj} P_j B_{fj} \space \forall \space f \in F
#'   \space \mathrm{(eqn \space 1c)} \\
#'   Z_{fj} \leq Y_{j} \space \forall \space j \in J \space \mathrm{(eqn \space
#'   1d)} \\
#'   \sum_{j = 0}^{J} Z_{fj} = 1 \space \forall \space f \in F \space
#'   \mathrm{(eqn \space 1e)} \\
#'   A_{ij} Y_{j} \leq X_{i} \space \forall \space i \in I, j \in J \space
#'   \mathrm{(eqn \space 1f)} \\
#'   E_{f} \geq 0, E_{f} \leq 1 \space \forall \space b \in B \space
#'   \mathrm{(eqn \space 1g)} \\
#'   X_{i}, Y_{j}, Z_{fj} \in [0, 1] \space \forall \space i \in I, j \in J, f
#'   \in F \space \mathrm{(eqn \space 1h)}
#'   }{
#'   Maximize (1 - prod_f^F E_f W_f) + sum_f^F (1 - E_f) W_f (eqn 1a);
#'   Subject to:
#'   sum_i^I C_i X_i <= m for all f in F (eqn 1b),
#'   E_f = 1 - sum_j^J Y_{fj} P_j B_{fj} for all f in F (eqn 1c),
#'   Z_{fj} <= Y_j for all j in J (eqn 1d),
#'   sum_j^J Z_{fj} = 1 for all f in F (eqn 1e),
#'   A_{ij} Y_{j} <= X_{i} for all i I, j in J (eqn 1f),
#'   E_f >= 0, E_f <= 1 for all f in F (eqn 1g),
#'   X_i, Y_j, Z_{fj} in [0, 1] for all i in I, j in J, f in F (eqn 1h)
#'   }
#'
#'  The objective (eqn 1a) is to maximize the probability that at least one
#'  feature will remain---given the probability that each feature will
#'  become extinct---plus the probability each feature will remain multiplied
#'  by their weights (noting that the feature weights default to zero).
#'  Constraint (eqn 1b) limits the maximum expenditure (i.e.
#'  ensures that the cost of the funded actions do not exceed the budget).
#'  Constraints (eqn 1c) calculate the probability that each feature
#'  will go extinct according to their allocated project.
#'  Constraints (eqn 1d) ensure that feature can only be allocated to projects
#'  that have all of their actions funded. Constraints (eqn 1e) state that each
#'  feature can only be allocated to a single project. Constraints (eqn 1f)
#'  ensure that a project cannot be funded unless all of its actions are funded.
#'  Constraints (eqns 1g) ensure that the probability variables
#'  (\eqn{E_f}) are bounded between zero and one. Constraints (eqns 1h) ensure
#'  that the action funding (\eqn{X_i}), project funding (\eqn{Y_j}), and
#'  project allocation (\eqn{Z_{fj}}) variables are binary.
#'
#'  Although this formulation is a mixed integer quadratically constrained
#'  programming problem (due to eqn 1c), it can be approximated using
#'  linear terms and then solved using commercial mixed integer programming
#'  solvers. This can be achieved by substituting the product of the feature
#'  extinction probabilities (eqn 1c) with the sum of the log feature extinction
#'  probabilities and using piecewise linear approximations (described in
#'  Hillier & Price 2005 pp. 390--392) to approximate the exponent of this term.
#'
#' @references
#'
#' Hillier FS & Price CC (2005) \emph{International series in operations
#' research & management science}. Springer.
#'
#' @inherit add_max_richness_objective seealso return
#'
#' @examples
#' #TODO
#'
#' @name add_max_persistence_objective
NULL

#' @rdname add_max_persistence_objective
#' @export
add_max_persistence_objective <- function(x, budget) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          assertthat::is.number(budget),
                          assertthat::noNA(budget),
                          isTRUE(budget >= 0))
  # add objective to problem
  x$add_objective(pproto(
    "MaximumPersistenceObjective",
    Objective,
    name = "Maximum persistence objective",
    data = list(feature_names = feature_names(x)),
    parameters = parameters(numeric_parameter("budget", budget,
                                              lower_limit = 0)),
    feature_phylogeny = function(self) {
      rake_phylogeny(self$data$feature_names,
                     rep(0, length(self$data$feature_names)), 1)
    },
    default_feature_weights = function(self) {
      stats::setNames(rep(0, length(self$data$feature_names)),
                      self$data$feature_names)
    },
    replace_feature_weights = function(self) {
      TRUE
    },
    evaluate = function(self, y, solution) {
      assertthat::assert_that(inherits(y, "ProjectProblem"),
                              inherits(solution, "tbl_df"))
      fp <- y$feature_phylogeny()
      bm <- branch_matrix(fp, FALSE)
      bo <- rcpp_branch_order(bm)
      w <- y$feature_weights()[y$feature_phylogeny()$tip.label]
      rcpp_evaluate_max_phylo_div_objective(
        y$action_costs(), y$pa_matrix(),
        y$epf_matrix()[, y$feature_phylogeny()$tip.label, drop = FALSE],
        bm[, bo, drop = FALSE], fp$edge.length[bo],
        rep(0, y$number_of_features()), w,
        methods::as(as.matrix(solution), "dgCMatrix"))
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ProjectProblem"))
      fp <- y$feature_phylogeny()
      bo <- rcpp_branch_order(branch_matrix(fp, FALSE))
      invisible(rcpp_apply_max_phylo_div_objective(
        x$ptr, y$action_costs(), self$parameters$get("budget"),
        fp$edge.length[bo]))
    }))
}
