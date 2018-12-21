#' @include internal.R
NULL

#' Project prioritization problem constraints
#'
#' A constraint can be added to a project prioritization \code{\link{problem}}
#' to ensure that solutions exhibit a specific characteristic.
#'
#' @details Constraints can be used to ensure that solutions exhibit
#'   a range of different characteristics. For instance, they can be
#'   used to lock in or lock out certain planning units from the solution, such
#'   as protected areas or degraded land (respectively). The following
#'   constraints are available.
#'
#'   The following constraints can be added to a project prioritization
#'   \code{\link{problem}}:
#'
#'   \describe{
#'
#'   \item{\code{\link{add_locked_in_constraints}}}{Add constraints to ensure
#'     that certain planning units are selected in the solution.}
#'
#'   \item{\code{\link{add_locked_out_constraints}}}{Add constraints to ensure
#'     that certain planning units are not selected in the solution.}
#'
#'  }
#'
#' @seealso \code{\link{decisions}}, \code{\link{objectives}},
#'  \code{\link{problem}}, \code{\link{solvers}}, \code{\link{targets}}.
#'
#' @examples
#' #TODO
#'
#' @name constraints
NULL
