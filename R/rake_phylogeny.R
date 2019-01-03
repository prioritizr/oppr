#' @include internal.R
NULL

#' Rake phylogeny
#'
#' Make a rake phylogeny using species names.
#'
#' @param x \code{character} \code{vector} of species names.
#'
#' @param species_weights \code{numeric} \code{vector} of weights. This must
#'   contain a value for each species. Defaults to assigning a weight of one for
#'   each species.
#'
#' @param branch_weight \code{numeric} weight for the single non-tip branch.
#'   Defaults to one.
#'
#' @details A rake phylogeny is essentially a star phylogeny, with an additional
#'   branch that connects to all the species.
#'
#' @return \code{\link[ape]{phylo}} tree object.
#'
#' @seealso \code{\link[ape]{stree}}.
#'
#' @examples
#' # load ape package
#' library(ape)
#'
#' # create rake phylogeny with five species and equal weights
#' p <- rake_phylogeny(letters[1:5])
#'
#' # plot the phylogeny, note that the "stick" of the rake is not depicted
#' plot(p)
#'
#' @noRd
rake_phylogeny <- function(x, species_weights = rep(1, length(x)),
                           branch_weight = 1) {
  # assert that arguments are valid
  assertthat::assert_that(is.character(x), length(x) > 0,
                          assertthat::noNA(x), anyDuplicated(x) == 0,
                          is.numeric(species_weights),
                          length(species_weights) == length(x),
                          assertthat::noNA(species_weights),
                          all(species_weights >= 0),
                          assertthat::is.number(branch_weight),
                          assertthat::noNA(branch_weight))

  # create rake phylogeny
  out <- ape::stree(length(x), type = "star", tip.label = x)
  out$tip.label <- x
  out$edge <- rbind(out$edge, c(length(x) + c(2L, 1L)))
  out$edge.length <- c(species_weights, branch_weight)
  out$Nnode <- out$Nnode + 1L

  # return output
  out
}
