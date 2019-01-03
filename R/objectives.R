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
#' of  the funded actions.
#'
#' \strong{Please note that failing to specify an objective before attempting
#' to solve a problem will return an error.}
#'
#' @details The following objectives can be added to a conservation planning
#'   \code{\link{problem}}:
#'
#'   \describe{
#'
#'   \item{\code{\link{add_min_set_objective}}}{Minimize the cost of the
#'     solution whilst ensuring that all targets are met. This objective is
#'     conceptually similar to that used in \emph{Marxan}---except that
#'     decisions are made on funding actions instead of purchasing areas
#'     for conservation.}
#'
#'   \item{\code{\link{add_max_persistence_objective}}}{Maximize the probability
#'     that at last one feature will persist into the future.}
#'
#'   \item{\code{\link{add_max_richness_objective}}}{Maximize the expected
#'     richness of the features. In other words, maximize the total
#'     number of species that are expected to persist.}
#'
#'
#'   \item{\code{\link{add_max_targets_met_objective}}}{Maximize the
#'     number of persistence targets that are met for the features.}
#'
#'   \item{\code{\link{add_max_phylo_div_objective}}}{Maximize the expected
#'     phylogenetic diversity of the features.}
#'
#'   }
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
