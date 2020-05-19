#' @include internal.R
NULL

#' @useDynLib oppr, .registration = TRUE
NULL

#' @importFrom ape plot.phylo
#' @export
NULL

#' oppr: Optimal Project Prioritization
#'
#'   The \pkg{oppr} \emph{R} package a decision support tool for prioritizing
#'   conservation projects. Prioritizations can be developed by maximizing
#'   expected feature richness, expected
#'   phylogenetic diversity, the number of features that meet persistence
#'   targets, or identifying a set of projects that meet persistence targets
#'   for minimal cost. Constraints (e.g. lock in
#'   specific actions) and feature weights can also be specified to further
#'   customize prioritizations. After defining a project prioritization
#'   problem, solutions can be obtained using exact algorithms, heuristic
#'   algorithms, or random processes. In particular, it is recommended to
#'   install the 'Gurobi' optimizer (available from
#'   \url{https://www.gurobi.com})
#'   because it can identify optimal solutions very quickly. Finally, methods
#'   are provided for comparing different prioritizations and evaluating their
#'   benefits.
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
#' \href{https://www.gurobi.com/downloads/end-user-license-agreement-academic/}{special license for no cost}. After installing the
#' \href{https://www.gurobi.com}{Gurobi} optimization suite, the \pkg{gurobi}
#'  package can then be installed (see instructions for \href{http://www.gurobi.com/documentation/8.1/quickstart_linux/r_installing_the_r_package.html}{Linux},
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_mac/r_installing_the_r_package.html}{Mac OSX}, and
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_windows/r_installing_the_r_package.html}{Windows} operating systems).
#'
#' @seealso Please refer to the package vignette for more information and worked
#'   examples. This can be accessed using the code
#'   \code{vignette("oppr")}.
#'
#' @name oppr
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # print project data
#' print(sim_projects)
#'
#' # print action data
#' print(sim_features)
#'
#' # print feature data
#' print(sim_actions)
#'
#' # build problem
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 400) %>%
#'      add_feature_weights("weight") %>%
#'      add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
#' \donttest{
#' # solve problem
#' s <- solve(p)
#'
#' # print output
#' print(s)
#'
#' # print which actions are funded in the solution
#' s[, sim_actions$name, drop = FALSE]
#'
#' # print the expected probability of persistence for each feature
#' # if the solution were implemented
#' s[, sim_features$name, drop = FALSE]
#'
#' # visualize solution
#' plot(p, s)
#' }
#' @docType package
NULL
