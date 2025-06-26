#' @include internal.R
NULL

#' @useDynLib oppr, .registration = TRUE
NULL

#' @importFrom ape plot.phylo
#' @export
NULL

#' oppr: Optimal Project Prioritization
#'
#'   The \pkg{oppr} *R* package a decision support tool for prioritizing
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
#'   <https://www.gurobi.com>)
#'   because it can identify optimal solutions very quickly. Finally, methods
#'   are provided for comparing different prioritizations and evaluating their
#'   benefits.
#'
#' @section Installation:
#' To make the most of this package, the \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{\pkg{ggtree}} and
#' \href{http://docs.gurobi.com/projects/optimizer/en/current/reference/r.html}{\pkg{gurobi}} R packages will need to be installed.
#' Since the \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{\pkg{ggtree}} package is exclusively available
#' at [Bioconductor](https://bioconductor.org)---and is not available on
#' [The Comprehensive R Archive Network](https://cran.r-project.org/)---please
#' execute the following command to install it:
#' `source("https://bioconductor.org/biocLite.R");biocLite("ggtree")`.
#' If the installation process fails, please consult the
#' [package's online documentation](https://bioconductor.org/packages/release/bioc/html/ggtree.html). To install the \pkg{gurobi} package, the
#' [Gurobi](https://www.gurobi.com) optimization suite will first need to
#' be installed (see <https://support.gurobi.com/hc/en-us/articles/4534161999889-How-do-I-install-Gurobi-Optimizer> for instructions). Although
#' [Gurobi](https://www.gurobi.com) is a commercial software, academics
#' can obtain a
#' [special license for no cost](https://www.gurobi.com/downloads/end-user-license-agreement-academic/). After installing the
#' [Gurobi](https://www.gurobi.com) optimization suite, the \pkg{gurobi}
#'  package can then be installed (see <https://support.gurobi.com/hc/en-us/articles/14462206790033-How-do-I-install-Gurobi-for-R> for instructions).
#'
#' @seealso Please refer to the package vignette for more information and worked
#'   examples. This can be accessed using the code
#'   `vignette("oppr")`.
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
#' \dontrun{
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
#'
#' @name oppr
#' @docType package
#' @aliases oppr-package
"_PACKAGE"
