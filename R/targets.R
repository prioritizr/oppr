#' @include internal.R pproto.R ProjectProblem-proto.R
NULL

#' Targets
#'
#' Targets are used to specify the minimum probability of persistence required
#' for each feature. \strong{Please note that only some objectives require
#' targets, and attempting to solve a problem that requires targets will throw
#' an error if targets are not supplied, and attempting to solve a problem that
#' does not require targets will throw a warning if targets are supplied.}
#'
#' @details The following functions can be used to specify targets for a
#'   project prioritization \code{\link{problem}}:
#'
#'   \describe{
#'
#'   \item{\code{\link{add_relative_targets}}}{
#'     Set targets as a proportion (between 0 and 1) of the maximum probability
#'     of persistence associated with the best project for feature. For
#'     instance, if the best project for a feature has an 80\% probability of
#'     persisting, setting a 50\% (i.e. \code{0.5}) relative target will
#'     correspond to a 40\% threshold probability of persisting.}
#'
#'   \item{\code{\link{add_absolute_targets}}}{
#'     Set targets for a project prioritization \code{\link{problem}} by
#'     specifying exactly what probability of persistence is required
#'     for each feature. For instance, setting an absolute target of 10%
#'     (i.e. \code{0.1}) corresponds to a threshold 10\% probability of
#'     persisting.}
#'
#'   \item{\code{\link{add_manual_targets}}}{Set targets manually.}
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
