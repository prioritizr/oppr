#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Specify the type of decisions
#'
#' Project prioritization problems involve making decisions on how
#' actions will be funded. If no decision is explicitly added to a
#' \code{\link{problem}}, then binary decisions will be used by default.
#'
#' @details
#'   Please note that only one type of decision is currently supported:
#'
#'   \describe{
#'
#'   \item{\code{\link{add_binary_decisions}}}{Add a binary decision to a
#'     project prioritization \code{problem}. This is the classic decision of
#'     either funding or not funding an action. If no decision is added to a
#'     problem object then this decision class will be used by default.}
#'
#'   }
#'
#' @seealso \code{\link{constraints}}, \code{\link{objectives}},
#'  \code{\link{problem}}, \code{\link{solvers}}, \code{\link{targets}},
#'  \code{\link{weights}}.
#'
#' @examples
#' #TODO
#'
#' @name decisions
NULL

add_default_decisions <- add_binary_decisions
