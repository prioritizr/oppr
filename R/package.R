#' @include internal.R
NULL

#' @useDynLib oppr, .registration = TRUE
NULL

#' oppr: Optimal Project Prioritization
#'
#' The \pkg{oppr} package a decision support tool for prioritizing
#' conservation projects. Prioritizations can be developed by maximizing
#' expected outcomes as a weighted sum (e.g., species richness), expected
#' phylogenetic diversity, the number of features that meet persistence
#' targets, or identifying a set of projects that meet persistence targets
#' for minimal cost. Constraints (e.g., lock in
#' specific actions) and feature weights can also be specified to further
#' customize prioritizations. After defining a project prioritization
#' problem, solutions can be obtained using exact algorithms, heuristic
#' algorithms, or random processes. In particular, it is recommended to
#' install the 'Gurobi' optimizer (available from
#' <https://www.gurobi.com>)
#' because it can identify optimal solutions very quickly. Finally, methods
#' are provided for comparing different prioritizations and evaluating their
#' benefits.
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
#' @details
#' This package has a vignette to showcase its usage. To view the
#' vignette, please use the code `vignette("oppr", package = "oppr")`.
#'
#' @section Citation:
#' Please cite the _oppr R_ package when using it in publications. To
#' cite the package, please use:
#'
#' Hanson JO, Schuster R, Strimas-Mackey M & Bennett JR (2019) Optimality in
#' prioritizing conservation projects. *Methods in Ecology & Evolution*,
#' 10: 1655--1663.
#'
#' @seealso
#' Useful links:
#' * Package website (<https://prioritizr.github.io/oppr/>)
#' * Source code repository (<https://github.com/prioritizr/oppr>)
#' * Report bugs (<https://github.com/prioritizr/oppr/issues>)
#'
#' @author
#'  Authors:
#' * Jeffrey O Hanson \email{jeffrey.hanson@uqconnect.edu.au} ([ORCID](https://orcid.org/0000-0002-4716-6134))
#' * Richard Schuster \email{richard.schuster@glel.carleton.ca} ([ORCID](https://orcid.org/0000-0003-3191-7869), maintainer)
#' * Matthew Strimas-Mackey \email{mstrimas@gmail.com} ([ORCID](https://orcid.org/0000-0001-8929-7776))
#' * Joseph Bennett \email{joseph.bennett@carleton.ca} ([ORCID](https://orcid.org/0000-0002-3901-9513))
#'
#'
#' @examplesIf oppr::run_example()
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
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 400) %>%
#'   add_feature_weights("weight") %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
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
#' @name oppr
#' @docType package
#' @aliases oppr-package
"_PACKAGE"

# avoid CRAN check NOTES due to R6 classes
# see: https://github.com/r-lib/R6/issues/230
if (getRversion() >= "2.15.1") utils::globalVariables(c("self"))
