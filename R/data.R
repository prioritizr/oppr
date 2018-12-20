#' @include internal.R
NULL

#' Simulated data
#'
#' Simulated data for prioritizing conservations projects.
#'
#' @details The data set contains the following objects:
#'
#' \describe{
#'
#'   \item{\code{sim_project_data}}{A \code{\link[tibble]{tibble}} containing
#'     data for six simulated conservation projects. Each row corresponds to a
#'     different project and each column contains information about the
#'     projects. This table contains the following columns:
#'
#'     \describe{
#'
#'       \item{\code{"name"}}{\code{character} name for each project.}
#'
#'       \item{\code{"success"}}{\code{numeric} probability of each project
#'         succeeding if it is funded.}
#'
#'       \item{\code{"S1"} ... \code{"S5"}}{\code{numeric} columns for each
#'         species (i.e. \code{"S1"}, \code{"S2"}, \code{"S3"}, \code{"S4"},
#'         \code{"S5"}, indicating the enhanced probability that each
#'         species will survive if it funded.}
#'
#'       \item{\code{"S1_action"} ... \code{"SN_action"}}{\code{logical}
#'         columns for each action, ranging from \code{"S1_action"} to
#'         \code{"SN_action"} where \code{N} is
#'         the number of actions (equal to the number of species in this
#'         simulated data), indicating if an action is associated with a
#'         project (\code{TRUE}) or not (\code{FALSE}).}
#'
#'     }}
#'
#'   \item{\code{sim_action_data}}{A \code{\link[tibble]{tibble}} containing
#'     data for six simulated actions. Each row corresponds to a
#'     different action and each column contains information about the
#'     actions. This table contains the following columns:
#'
#'     \describe{
#'
#'       \item{\code{"name"}}{\code{character} name for each action.}
#'
#'       \item{\code{"cost"}}{\code{numeric} cost for each action.}
#'
#'       \item{\code{"locked_in"}}{\code{logical} indicating if certain
#'         actions should be locked into the solution.}
#'
#'       \item{\code{"locked_out"}}{\code{logical} indicating if certain
#'         actions should be locked out of the solution.}
#'
#'     }}
#'
#'   \item{\code{sim_species_data}}{A \code{\link[tibble]{tibble}} containing
#'     data for six simulated species. Each row corresponds to a
#'     different species and each column contains information about the
#'     species. This table contains the following columns:
#'
#'     \describe{
#'
#'       \item{\code{"name"}}{\code{character} name for each species.}
#'
#'       \item{\code{"weight"}}{\code{numeric} weight for each species.}
#'
#'     }}
#'
#'     \item{tree}{\code{\link[ape]{phylo}} phylogenetic tree for the species.}
#'
#' }
#' @aliases sim_project_data sim_action_data sim_organization_data sim_tree sim_species_data
#'
#' @format \describe{
#'   \item{sim_project_data}{\code{\link[tibble]{tibble}} object.}
#'   \item{sim_action_data}{\code{\link[tibble]{tibble}} object.}
#'   \item{sim_species_data}{\code{\link[tibble]{tibble}} object.}
#'   \item{sim_tree}{\code{\link[ape]{phylo}} object.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' # load data
#' data(sim_project_data, sim_action_data, sim_species_data, sim_tree)
#'
#' # print project data
#' print(sim_project_data)
#
#' # print action data
#' print(sim_action_data)
#'
#' # print species data
#' print(sim_species_data)
#
#' # print phylogenetic tree
#' print(sim_tree)
#'
#' # plot phylogenetic tree
#' plot(sim_tree)
#'
#' @name sim_data
NULL

#' @rdname sim_data
#' @usage data(sim_project_data)
"sim_project_data"

#' @rdname sim_data
#' @usage data(sim_action_data)
"sim_action_data"

#' @rdname sim_data
#' @usage data(sim_species_data)
"sim_action_data"

#' @rdname sim_data
#' @usage data(sim_tree)
"sim_tree"
