#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add maximum phylogenetic diversity objective
#'
#' Set the objective of a project prioritization \code{\link{problem}} to
#' maximize the phylogenetic diversity that is expected to persist into the
#' future, whilst ensuring that the cost of the solution is within a
#' pre-specified budget (Bennett \emph{et al.} 2014, Faith 2008). Furthermore,
#' weights can also be used to specify the relative importance of conserving
#' specific features (see \code{\link{add_feature_weights}}).
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param budget \code{numeric} budget for funding actions.
#'
#' @param tree \code{\link[ape]{phylo}} phylogenetic tree describing the
#'   evolutionary relationships between the features. Note that every
#'   feature must be accounted for in the argument to \code{tree}.
#'
#' @details A problem objective is used to specify the overall goal of the
#'   project prioritization problem. Please note that all project
#'   prioritization problems formulated in the \pkg{ppr} package require
#'   the addition of objectives---failing to do so will return an error
#'   message when attempting to solve problem.
#'
#' @section: Formulation:
#'   Here, the maximum phylogenetic diversity objective seeks to find the set
#'   of actions that maximizes the expected amount of evolutionary history
#'   that is expected to persist into the future given the evolutionary
#'   relationships between the features (e.g. populations, species).
#'    Let \eqn{I} represent the set of conservation actions (indexed by
#'   \eqn{i}). Let \eqn{C_i} denote the cost for funding action \eqn{i}, and
#'   let \eqn{m} denote the maximum expenditure (i.e. the budget). Also,
#'   let \eqn{F} represent each feature (indexed by \eqn{f}), \eqn{W_f}
#'   represent the weight for each feature \eqn{f} (defaults to zero for
#'   each feature unless specified otherwise), and \code{E_f}
#'   denote the probability that each feature will go extinct given the funded
#'   conservation projects.
#'
#'   To describe the
#'   evolutionary relationships between the features \eqn{f \in F}{f in F},
#'   consider a phylogenetic tree that contains features \eqn{f \in F}{f in F}
#'   with branches of known lengths. This tree can be described using
#'   mathematical notation by letting \eqn{B} represent the branches (indexed by
#'   \eqn{b}) with lengths \eqn{L_b} and letting \eqn{T_{bf}} indicate which
#'   features \eqn{f \in F}{f in F} are associated with which phylogenetic
#'   branches \eqn{b \in B}{b in B} using zeros and ones. Ideally, the set of
#'   features \eqn{F} would contain all of the species in the study
#'   area---including non-threatened species---to fully account for the benefits
#'   for funding different actions.
#'
#'   To guide the prioritization, the conservation actions are organized into
#'   conservation projects. Let \eqn{J} denote the set of conservation projects
#'   (indexed by \eqn{j}), and let \eqn{A_{ij}} denote which actions
#'   \eqn{i \in I}{i in I} comprise each conservation project
#'   \eqn{j \in J}{j in J} using zeros and ones. Next, let \eqn{P_j} represent
#'   the probability of project \eqn{j} being successful if it is funded. Also,
#'   let \eqn{B_{fj}} denote the enhanced probability that each feature
#'   \eqn{s \in S}{s in S} associated with the project \eqn{j \in J}{j in J}
#'   will persist if all of the actions that comprise project \eqn{j} are funded
#'   and that project is allocated to feature \eqn{f}.
#'
#'   The binary control variables \eqn{X_i} in this problem indicate whether
#'   each project \eqn{i \in I}{i in I} is funded or not. The decision
#'   variables in this problem are the \eqn{Y_{j}}, \eqn{Z_{fj}}, \eqn{E_f},
#'   and \eqn{R_b} variables.
#'   Specifically, the binary \eqn{Y_{j}} variables indicate if project \eqn{j}
#'   is funded or not based on which actions are funded; the binary
#'   \eqn{Z_{sj}} variables indicate if project \eqn{j} is used to manage
#'   feature \eqn{s} or not; the semi-continuous \eqn{E_f} variables
#'   denote the probability that feature \eqn{f} will go extinct; and
#'   the semi-continuous \eqn{R_b} variables denote the probability that
#'   phylogenetic branch \eqn{b} will remain in the future.
#'
#'   Now that we have defined all the data and variables, we can formulate
#'   the problem. For convenience, let the symbol used to denote each set also
#'   represent its cardinality (e.g. if there are ten features, let \eqn{F}
#'   represent the set of ten features and also the number ten).
#'
#' \deqn{
#'   \mathrm{Maximize} \space (\sum_{b = 0}^{B} L_b R_b) + \sum_{f}^{F}
#'   (1 - E_f) W_f \space \mathrm{(eqn \space 1a)} \\
#'   \mathrm{Subject \space to} \space
#'   \sum_{i = 0}^{I} C_i \leq m \space \mathrm{(eqn \space 1b)} \\
#'   R_b = 1 - \prod_{f = 0}^{F} ifelse(T_{bf} == 1, \space E_f, \space
#'   1) \space \forall \space b \in B \space \mathrm{(eqn \space 1c)} \\
#'   E_f = 1 - \sum_{j = 0}^{J} Z_{fj} P_j B_{fj} \space \forall \space f \in F
#'   \space \mathrm{(eqn \space 1d)} \\
#'   Z_{fj} \leq Y_{j} \space \forall \space j \in J \space \mathrm{(eqn \space
#'   1e)} \\
#'   \sum_{j = 0}^{J} Z_{fj} = 1 \space \forall \space f \in F \space
#'   \mathrm{(eqn \space 1f)} \\
#'   A_{ij} Y_{j} \leq X_{i} \space \forall \space i \in I, j \in J \space
#'   \mathrm{(eqn \space 1g)} \\
#'   E_{f} \geq 0, E_{f} \leq 1 \space \forall \space b \in B \space
#'   \mathrm{(eqn \space 1h)} \\
#'   X_{i}, Y_{j}, Z_{fj} \in [0, 1] \space \forall \space i \in I, j \in J, f
#'   \in F \space \mathrm{(eqn \space 1i)}
#'   }{
#'   Maximize (sum_b^B L_b R_b) + sum_f^F (1 - E_f) W_f (eqn 1a);
#'   Subject to:
#'   sum_i^I C_i X_i <= m for all f in F (eqn 1b),
#'   R_b = 1 - prod_f^F ifelse(T_{bf} == 1, E_f, 1) for all b in B (eqn 1c),
#'   E_f = 1 - sum_j^J Y_{fj} P_j B_{fj} for all f in F (eqn 1d),
#'   Z_{fj} <= Y_j for all j in J (eqn 1e),
#'   sum_j^J Z_{fj} = 1 for all f in F (eqn 1f),
#'   A_{ij} Y_{j} <= X_{i} for all i I, j in J (eqn 1g),
#'   E_f >= 0, E_f >= 1 for all f in F (eqn 1h),
#'   X_i, Y_j, Z_{fj} in [0, 1] for all i in I, j in J, f in F (eqn 1i)
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
#'  \eqn{E_s}) are bounded between zero and one. Constraints (eqns 1h) ensure
#'  that the action funding (\eqn{X_j}), project funding (\eqn{Y_j}), and
#'  project allocation (\eqn{Z_{fj}}) variables are binary.
#'
#' @inherit add_min_set_objective seealso return
#'
#' @references
#' Bennett JR, Elliott G, Mellish B, Joseph LN, Tulloch AI,
#' Probert WJ, ... & Maloney R (2014) Balancing phylogenetic diversity
#' and species numbers in conservation prioritization, using a case study of
#' threatened species in New Zealand. \emph{Biological Conservation},
#' \strong{174}: 47--54.
#'
#' Faith DP (2008) Threatened species and the potential loss of
#' phylogenetic diversity: conservation scenarios based on estimated extinction
#' probabilities and phylogenetic risk analysis. \emph{Conservation Biology},
#' \strong{22}: 1461--1470.

#' @examples
#' #TODO
#'
#' @name add_max_phylo_div_objective
NULL

#' @rdname add_max_phylo_div_objective
#' @export
add_max_phylo_div_objective <- function(x, budget, tree) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          assertthat::is.number(budget),
                          assertthat::noNA(budget),
                          isTRUE(budget >= 0),
                          inherits(tree, "phylo"),
                          is_valid_phylo(tree),
                          setequal(tree$tip.label, x$feature_names()))
  # add objective to problem
  x$add_objective(pproto(
    "MaximumPhyloObjective",
    Objective,
    name = "Maximum phylogenetic diversity objective",
    data = list(feature_names = feature_names(x), tree = tree),
    parameters = parameters(numeric_parameter("budget", budget,
                                              lower_limit = 0)),
    feature_phylogeny = function(self) {
      self$get_data("tree")
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ProjectProblem"))
      fp <- y$feature_phylogeny()
      bo <- rcpp_branch_order(branch_matrix(fp, FALSE))
      invisible(rcpp_apply_max_phylo_objective(x$ptr, y$action_costs(),
                                               self$parameters$get("budget"),
                                               fp$edge.length[bo]))
    }))
}
