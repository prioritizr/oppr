#' @include internal.R
NULL

#' Branch matrix
#'
#' Phylogenetic trees depict the evolutionary relationships between different
#' species. Each branch in a phylogenetic tree represents a period of
#' evolutionary history. Species that are connected to the same branch
#' share the same period of evolutionary history represented by the branch.
#' This function creates
#' a matrix that shows which species are connected with which branches. In
#' other words, it creates a matrix that shows which periods of evolutionary
#' history each species has experienced.
#'
#' @param x \code{\link[ape]{phylo}} tree object.
#'
#' @param assert_validity \code{logical} value (i.e. \code{TRUE} or \code{FALSE}
#'   indicating if the argument to \code{x} should be checked for validity.
#'   Defaults to \code{TRUE}.
#'
#' @param ... not used.
#'
#'
#' @return \code{\link[Matrix]{dgCMatrix-class}} sparse matrix object. Each row
#'   corresponds to a different species. Each column corresponds to a different
#'   branch. Species that inherit from a given branch are indicated with a one.
#'
#' @name branch_matrix
#'
#' @rdname branch_matrix
#'
#' @examples
#' # load Matrix package to plot matrices
#' library(Matrix)
#'
#' # load data
#' data(sim_tree)
#'
#' # generate species by branch matrix
#' m <- branch_matrix(sim_tree)
#'
#' # plot data
#' par(mfrow = c(1,2))
#' plot(sim_tree, main = "phylogeny")
#' image(m, main = "branch matrix")
#'
#' @export
branch_matrix <- function(x, ...) UseMethod("branch_matrix")

#' @rdname branch_matrix
#' @method branch_matrix default
#' @export
branch_matrix.default <- function(x, ...)
  rcpp_branch_matrix(methods::as(x, "phylo"))

#' @rdname branch_matrix
#' @method branch_matrix phylo
#' @export
branch_matrix.phylo <- function(x, assert_validity = TRUE, ...) {
  # check that tree is valid and return error if not
  assertthat::assert_that(assertthat::is.flag(assert_validity))
  if (assert_validity)
    assertthat::assert_that(is_valid_phylo(x))
  # generate matrix
  rcpp_branch_matrix(x)
}
