#' @include internal.R
NULL

#' Project prioritization problem constraints
#'
#' A constraint can be added to a project prioritization \code{\link{problem}}
#' to ensure that solutions exhibit a specific characteristic.
#'
#' @details
#'   The following constraints can be added to a project prioritization
#'   \code{\link{problem}}:
#'
#'   \describe{
#'
#'   \item{\code{\link{add_locked_in_constraints}}}{Add constraints to ensure
#'     that certain actions are prioritized for funding.}
#'
#'   \item{\code{\link{add_locked_out_constraints}}}{Add constraints to ensure
#'     that certain actions are not prioritized for funding.}
#'
#'  }
#'
#' @seealso \code{\link{decisions}}, \code{\link{objectives}},
#'  \code{\link{problem}}, \code{\link{solvers}}, \code{\link{targets}},
#'  \code{\link{weights}}.
#'
#' @examples
#' #TODO
#'
#' @name constraints
NULL
