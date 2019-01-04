#' @include internal.R pproto.R Objective-proto.R
NULL

#' Problem objective
#'
#' An objective is used to specify the overall goal of a project prioritization
#' \code{\link{problem}}. All project prioritization problems involve
#' minimizing or maximizing some kind of objective. For instance, the decision
#' maker may require a funding scheme that maximizes the total number of
#' species that are expected to persist into the future whilst ensuring that
#' the total cost of the funded actions does not exceed a budget.
#' Alternatively, the planner may require a solution that ensures that
#' each species meets a target level of persistence whilst minimizing the cost
#' of  the funded actions. A project prioritization
#' \code{\link{problem}} \strong{must} have a specified objective before it can
#' be solved, and attempting to solve a problem which does not have
#' a specified objective will throw an error.
#'
#' @details The following objectives can be added to a conservation planning
#'   \code{\link{problem}}:
#'
#'   \describe{
#'
#'   \item{\code{\link{add_min_set_objective}}}{
#'     Minimize the cost of the solution whilst ensuring that all targets are
#'     met. This objective is conceptually similar to that used in \emph{Marxan}
#'     (Ball, Possingham & Watts 2009).}
#'
#'   \item{\code{\link{add_max_prob_persistence_objective}}}{
#'     Maximize the chance that at least one feature will persist into the
#'     future, whilst ensuring that the cost of the solution is within a
#'     pre-specified budget.}
#'
#'   \item{\code{\link{add_max_sum_persistence_objective}}}{
#'     Maximize the total number of features that are expected to persist,
#'     whilst ensuring that the cost of the solution is within a pre-specified
#'     budget (Joseph, Maloney & Possingham 2009).}
#'
#'   \item{\code{\link{add_max_targets_met_objective}}}{
#'     Maximize the total number of persistence targets met for the features,
#'     whilst ensuring that the cost of the solution is within a pre-specified
#'     budget (Chades \emph{et al.} 2015).}
#'
#'   \item{\code{\link{add_max_phylo_div_objective}}}{
#'     Maximize the phylogenetic diversity that is expected to persist into the
#'     future, whilst ensuring that the cost of the solution is within a
#'     pre-specified budget (Bennett \emph{et al.} 2014, Faith 2008).}
#'
#'   }
#'
#' @references
#' Ball IR, Possingham HP & Watts M (2009) Marxan and relatives: software for
#' spatial conservation prioritisation.
#' \emph{Spatial conservation prioritisation: Quantitative methods and
#' computational tools}, 185-195.
#'
#' Bennett JR, Elliott G, Mellish B, Joseph LN, Tulloch AI,
#' Probert WJ, Di Fonzo MMI, Monks JM, Possingham HP & Maloney R (2014)
#' Balancing phylogenetic diversity
#' and species numbers in conservation prioritization, using a case study of
#' threatened species in New Zealand. \emph{Biological Conservation},
#' \strong{174}: 47--54.
#'
#' Chades I, Nicol S, van Leeuwen S, Walters B, Firn J, Reeson A, Martin TG &
#' Carwardine J (2015) Benefits of integrating complementarity into priority
#' threat management. \emph{Conservation Biology}, \strong{29}, 525--536.
#'
#' Faith DP (2008) Threatened species and the potential loss of
#' phylogenetic diversity: conservation scenarios based on estimated extinction
#' probabilities and phylogenetic risk analysis. \emph{Conservation Biology},
#' \strong{22}: 1461--1470.
#'
#' Joseph LN, Maloney RF & Possingham HP (2009) Optimal allocation of
#' resources among threatened species: A project prioritization protocol.
#' \emph{Conservation Biology}, \strong{23}, 328--338.
#'
#' @seealso \code{\link{constraints}}, \code{\link{decisions}},
#'   \code{\link{problem}}, \code{\link{solvers}}, \code{\link{targets}},
#'   \code{\link{weights}}.
#'
#' @examples
#' #TODO
#' @name objectives
NULL

add_default_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ProjectProblem"))
  # throw error because objectives must be explicitly defined
  stop("problem is missing an objective and this must be explicitly defined")
}
