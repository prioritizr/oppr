#' @include internal.R pproto.R ProjectProblem-proto.R
NULL

#' Targets
#'
#' Targets are used to specify the minimum probability of persistence required
#' for each feature. \strong{Please note that only some objectives require
#' targets, and attempting to solve a problem that requires targets will throw
#' an error if targets are not supplied.}
#'
#' @details
#'   Please note that only one type of target is currently supported:
#'
#'   \describe{
#'
#'   \item{\code{\link{add_persistence_targets}}}{Set minimum persistence
#'      targets.}
#'
#'   }
#'
#' @seealso \code{\link{constraints}}, \code{\link{decisions}},
#'  \code{\link{objectives}}, \code{\link{problem}},
#'  \code{\link{solvers}}.
#'
#' @examples
#' #TODO
#'
#' @name targets
NULL

add_default_targets <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"))
  # throw error because targets must be chosen by the user
  stop("problem is missing targets and they must be explicitly defined")
}
