#' @include internal.R
NULL

#' Simulated multi-objective data
#'
#' Simulated data for prioritizing conservation projects based on multiple
#' objectives.
#'
#' @usage data(sim_multi_actions)
#' @usage data(sim_multi_projects)
#' @usage data(sim_multi_features)
#' @usage data(sim_multi_tree)
#'
#' @format
#' \describe{
#' \item{sim_multi_projects}{`list` of [tibble::tibble()] objects.}
#' \item{sim_multi_actions}{[tibble::tibble()] object.}
#' \item{sim_multi_features}{`list` of [tibble::tibble()] objects.}
#' \item{sim_multi_tree}{`list` of [ape::phylo()] objects.}
#' }
#'
#' @details
#' The data set contains the following objects.
#'
#' \describe{
#'
#' \item{`"sim_multi_projects"`}{
#' A `list` of three [tibble::tibble()] objects containing
#' data for simulated conservation projects. Each [tibble::tibble()] object
#' corresponds to a different objective. Within each of these
#' [tibble::tibble()] objects, each row corresponds to a
#' different project and each column contains information about the
#' projects. These tables contains the following columns.
#'
#' \describe{
#'
#' \item{`"name"`}{
#' These columns contain the `character` name for each project.
#' }
#'
#' \item{`"success"`}{
#' These columns contain the `numeric` probability of each project succeeding
#' if it is funded.
#' }
#'
#' \item{`"F1"` ... `"F5"`}{
#' These columns contain `numeric` values for each
#' feature (i.e., `"F1"`, `"F2"`, `"F3"`, `"F4"`, ...,
#' `"F10"`) that indicate the probability that the feature will
#' persist if each project is successfully completed.
#' Missing values (`NA`) indicate that a feature does not benefit from a
#' project being funded.
#' }
#'
#' \item{`"A1"` ... `"A15"`}{
#' These columns contain `logical` (`TRUE`/`FALSE`) values for each action
#' (i.e., `"A1"`, `"A2"`, ..., `"A15"`) indicating if
#' the action is associated with each project or not.
#' }
#'
#' \item{`"baseline_action"`}{
#' These columns contain `logical` (`TRUE`/`FALSE`) values for each
#' project indicating if the project is the baseline project or not.
#' }
#'
#' }
#' }
#'
#' \item{`sim_multi_actions`}{
#' A [tibble::tibble()] object containing
#' data for 15 simulated actions. Each row corresponds to a
#' different action and each column contains information about the
#' actions. This table contains the following columns.
#'
#' \describe{
#'
#' \item{`"name"`}{
#' This column contains the `character` name for each action.
#' }
#'
#' \item{`"cost"`}{
#' This column contains the `numeric` cost for each action.
#' }
#'
#' \item{`"locked_in"`}{
#' This column contains `logical` values indicating if particular actions
#' should  be locked into the solution.
#' }
#'
#' \item{`"locked_out"`}{
#' This column contains `logical` values indicating if particular actions
#' should be locked out of the solution.
#' }
#'
#' }
#' }
#'
#' \item{`sim_multi_features`}{
#' A `list` of three [tibble::tibble()] objects containing
#' data for ten simulated features.  Each [tibble::tibble()] object
#' corresponds to a different objective. With each [tibble::tibble()] object,
#' each row corresponds to a different feature and each column contains
#' information about the features. These tables contains the following columns.
#'
#' \describe{
#'
#' \item{`"name"`}{
#' These columns contain `character` values denoting name for each feature.
#' }
#'
#' \item{`"weight"`}{
#' These columns contain `numeric` values denoting weight for each feature.
#' }
#'
#' }
#' }
#'
#' \item{sim_multi_trees}{
#' A `list` of [ape::phylo()] phylogenetic tree objects for the features
#' associated with each of the three objective.
#' }
#'
#' }
#'
#' @aliases sim_multi_projects sim_multi_actions sim_multi_features sim_multi_tree
#'
#' @keywords datasets
#'
#' @docType data
#'
#' @examples
#' # load data
#' data(sim_multi_projects)
#' data(sim_multi_actions)
#' data(sim_multi_features)
#' data(sim_multi_tree)
#'
#' # print project data
#' print(sim_multi_projects)
#'
#' # print action data
#' print(sim_multi_actions)
#'
#' # print feature data
#' print(sim_multi_features)
#'
#' # print phylogenetic trees
#' print(sim_multi_tree)
#'
#' @name sim_multi_data
NULL
