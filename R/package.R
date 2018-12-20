#' @include internal.R
NULL

#' @useDynLib ppr, .registration = TRUE
NULL

#' @importFrom ape plot.phylo
#' @export
NULL

#' \pkg{ppr}: Project Prioritization
#'
#' @section Description:
#' The \pkg{ppr} \emph{R} package is a decision support tool for prioritizing
#' conservation projects for funding. Prioritizations can be developed by
#' maximizing expected species richness, expected phylogenetic diversity, the
#' number of species that meet persistence targets, or identifying a set of
#' projects that meet persistence targets for minimal cost. After defining the
#' optimization problem, prioritizations can be generated using exact
#' algorithms---which are guaranteed to find optimal solutions---or
#' conventional heuristic algorithms. Finally, this package provides methods
#' for comparing different prioritizations and evaluating their benefits.
#'
#' @section Installation:
#' To make the most of this package, the \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{\pkg{ggtree}} and
#' \href{http://www.gurobi.com/documentation/8.1/refman/r_api_overview.html}{\pkg{gurobi}} R packages will need to be installed.
#' Since the \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{\pkg{ggtree}} package is exclusively available
#' at \href{https://bioconductor.org}{Bioconductor}---and is not available on
#' \href{https://cran.r-project.org/}{The Comprehensive R Archive Network}---please
#' execute the following command to install it:
#' \code{source("https://bioconductor.org/biocLite.R");biocLite("ggtree")}.
#' If the installation process fails, please consult the
#' \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{package's online documentation}. To install the \pkg{gurobi} package, the
#' \href{https://www.gurobi.com}{Gurobi} optimization suite will first need to
#' be installed (see instructions for \href{http://www.gurobi.com/documentation/8.1/quickstart_linux/software_installation_guid.html}{Linux},
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_mac/software_installation_guid.html}{Mac OSX}, and
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_windows/software_installation_guid.html}{Windows} operating systems). Although
#' \href{https://www.gurobi.com}{Gurobi} is a commercial software, academics
#' can obtain a
#' \href{https://user.gurobi.com/download/licenses/free-academic}{special license for no cost}. After installing the
#' \href{https://www.gurobi.com}{Gurobi} optimization suite, the \pkg{gurobi}
#'  package can then be installed (see instructions for \href{http://www.gurobi.com/documentation/8.1/quickstart_linux/r_installing_the_r_package.html}{Linux},
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_mac/r_installing_the_r_package.html}{Mac OSX}, and
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_windows/r_installing_the_r_package.html}{Windows} operating systems).
#'
#' @seealso Please refer to the package vignette for more information and worked
#'   examples. This can be accessed using the code
#'   \code{vignette("ppr")}.
#'
#' @name ppr
#'
#' @docType package
NULL
