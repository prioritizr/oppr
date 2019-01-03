#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Add binary decisions
#'
#' Add a binary decision to a project prioritization \code{\link{problem}}.
#' This is the conventional decision of either prioritizing funding
#' for a management action or not.
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @details Project prioritization problems involve making decisions on
#'   about which management actions should be funded or not. Only a
#'   single decision should be added to a \code{ProjectProblem} object.
#'   If no decision is added to a problem then this decision class will
#'   be used by default. Currently, this is the only supported decision type.
#'
#' @return \code{\link{ProjectProblem-class}} object with the decisions
#'   added to it.
#'
#' @seealso \code{\link{decisions}}.
#'
#' @examples
#' #TODO
#' @name add_binary_decisions
NULL

#' @rdname add_binary_decisions
#' @export
add_binary_decisions <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ProjectProblem"))
  # add decision
  x$add_decisions(
    pproto("BinaryDecision",
           Decision,
           name = "Binary decision",
           apply = function(self, x) {
             assertthat::assert_that(inherits(x,
                                     "OptimizationProblem"))
             invisible(rcpp_apply_decisions(x$ptr, "B", 0, 1))
           }))
}
