#' @include internal.R
NULL

#' Solution statistics
#'
#' Calculate statistics describing a solution to a project prioritization
#' \code{\link{problem}}.
#'
#' @param x project prioritization \code{\link{problem}}.
#'
#' @param solution \code{\link[base]{data.frame}} or
#'   \code{\link[tibble]{tibble}} table containing the solutions. Here,
#'   rows correspond to different solutions and columns correspond to
#'   different actions. Each column in the argument to \code{solution} should
#'   be named according to a different action in \code{x}.
#'   Cell values indicate if an action is funded in a given solution or not,
#'   and should be either zero or one.
#'
#' @return A \code{\link[tibble]{tibble}} table containing the following
#'   columns:
#'
#'   \describe{
#'
#'   \item{\code{"cost"}}{\code{numeric} cost of each solution.}
#'
#'   \item{\code{"obj"}}{\code{numeric} objective value for each solution.
#'     This is calculated using the objective function defined for the
#'     argument to \code{x}.}
#'
#'   \item{\code{x$feature_names()}}{\code{numeric} column for each
#'     feature indicating the probability that it will persist into
#'     the future given each solution.}
#'
#'   }
#'
#' @seealso \code{\link{objectives}}.
#'
#' @examples
#' #TODO
#
#' @export
solution_statistics <- function(x, solution) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          inherits(solution, "data.frame"),
                          all(assertthat::has_name(solution, x$action_names())))
  assertthat::assert_that(!is.Waiver(x$objective),
    msg = "argument to x does not have an objective specified.")
  if (!inherits(solution, "tbl_df"))
    solution <- tibble::as_tibble(solution)
  # calculate cost and objective values
  out <- tibble::tibble(
    cost = rowSums(as.matrix(solution[, x$action_names()]) *
                   matrix(x$action_costs(), byrow = TRUE, ncol = ncol(solution),
                          nrow = nrow(solution))),
    obj = x$objective$evaluate(x, solution[, x$action_names()]))
  # add in columns for feature persistences
  out <- tibble::as_tibble(cbind(out, stats::setNames(as.data.frame(
    rcpp_expected_persistences(
      x$pa_matrix(), x$epf_matrix(),
      methods::as(diag(x$number_of_features()), "dgCMatrix"),
      methods::as(as.matrix(solution[, x$action_names()]), "dgCMatrix"))),
      x$feature_names())))
  # return output
  out
}
