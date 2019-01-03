#' @include internal.R pproto.R Parameters-proto.R ProjectModifier-proto.R
NULL

#' @export
if (!methods::isClass("Weight")) methods::setOldClass("Weight")
NULL

#' Weight prototype
#'
#' This prototype is used to represent the weights used when making a
#' prioritization. This prototype inherits from the
#' \code{\link{ProjectModifier-class}}. \strong{This class represents a
#' recipe, to actually add targets to a planning problem, see the help page on
#' \code{\link{weights}}. Only experts should use this class directly.}
#'
#' @seealso \code{\link{ProjectModifier-class}}.
#'
#' @name Weight-class
#'
#' @aliases Weight
NULL

#' @export
Weight <- pproto("Weight", ProjectModifier)

add_default_weights <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ProjectProblem"),
                          !is.Waiver(x$objective))
  add_feature_weights(x, x$objective$default_feature_weights())
}
