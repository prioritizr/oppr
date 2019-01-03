#' @include internal.R
NULL

#' Star phylogeny
#'
#' Make a star phylogeny using species names and, optionally, species weights.
#'
#' @param x \code{character} \code{vector} of species names.
#'
#' @param weights \code{numeric} \code{vector} of weights. This must contain
#'   a value for each species. Defaults to assigning an equal weight for
#'   each species.
#'
#' @return \code{\link[ape]{phylo}} tree object.
#'
#' @seealso \code{\link[ape]{stree}}.
#'
#' @examples
#' # load ape package
#' library(ape)
#'
#' # create star phylogeny with five species and equal weights
#' p1 <- star_phylogeny(letters[1:5])
#'
#' # plot the phylogeny
#' plot(p1)
#'
#' # create star phylogeny with five species and varying weights
#' p2 <- star_phylogeny(letters[1:5], weights = seq(1, 5))
#'
#' # plot the phylogeny
#' plot(p2)
#'
#' @noRd
star_phylogeny <- function(x, weights = rep(1, length(x))) {
  # assert that arguments are valid
  assertthat::assert_that(is.character(x), length(x) > 0,
                          assertthat::noNA(x), anyDuplicated(x) == 0,
                          is.numeric(weights), length(weights) == length(x),
                          assertthat::noNA(weights), all(weights >= 0))

  # create star phylogeny
  out <- ape::stree(length(x), type = "star", tip.label = x)
  out$tip.label <- x
  out$edge.length <- weights

  # return output
  out
}
