#' @include internal.R pproto.R Parameters-proto.R ProjectModifier-proto.R
NULL

#' @export
if (!methods::isClass("Target")) methods::setOldClass("Target")
NULL

#' Target prototype
#'
#' This prototype is used to represent the targets used when making a
#' prioritization. This prototype inherits from the
#' \code{\link{ProjectModifier-class}}. \strong{This class represents a
#' recipe, to actually add targets to a planning problem, see the help page on
#' \code{\link{targets}}. Only experts should use this class directly.}
#'
#' @seealso \code{\link{ProjectModifier-class}}.
#'
#' @name Target-class
#'
#' @aliases Target
NULL

#' @export
Target <- pproto("Target", ProjectModifier)