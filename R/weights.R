#' @include internal.R pproto.R ProjectProblem-proto.R
NULL

#' Weights
#'
#' Weights are used to specify the relative importance for specific
#' features persisting into the future. Please note that only some
#' objectives require weights, and attempting to solve a problem that does not
#' require weights will throw a warning and the weights will be ignored.
#'
#' @details Currently, only one function can be used to specify weights:
#'
#'   \describe{
#'
#'   \item{\code{\link{add_feature_weights}}}{
#'     Set feature weights for a project prioritization \code{\link{problem}}.}
#'
#'   }
#'
#' @seealso \code{\link{constraints}}, \code{\link{decisions}},
#'  \code{\link{objectives}}, \code{\link{problem}},
#'  \code{\link{solvers}}, \code{\link{targets}}.
#'
#' @examples
#' #TODO
#'
#' @name weights
NULL
