#' @include internal.R
NULL

#' Simulated data
#'
#' Simulated data for prioritizing conservation projects.
#'
#' @usage data(sim_actions)
#' @usage data(sim_projects)
#' @usage data(sim_features)
#' @usage data(sim_tree)
#'
#' @format
#' \describe{
#'   \item{sim_projects}{[tibble::tibble()] object.}
#'   \item{sim_actions}{[tibble::tibble()] object.}
#'   \item{sim_features}{[tibble::tibble()] object.}
#'   \item{sim_tree}{[ape::phylo()] object.}
#' }
#'
#' @details The data set contains the following objects:
#'
#' \describe{
#'   \item{`sim_projects`}{A [tibble::tibble()] object containing
#'     data for six simulated conservation projects. Each row corresponds to a
#'     different project and each column contains information about the
#'     projects. This table contains the following columns:
#'     \describe{
#'       \item{`"name"`}{`character` name for each project.}
#'       \item{`"success"`}{`numeric` probability of each project
#'         succeeding if it is funded.}
#'       \item{`"F1"` ... `"F5"`}{`numeric` columns for each
#'         feature (i.e. `"F1"`, `"F2"`, `"F3"`, `"F4"`,
#'         `"F5"`, indicating the enhanced probability that each
#'         feature will survive if it funded. Missing values (`NA`)
#'         indicate that a feature does not benefit from a project being
#'         funded.}
#'       \item{`"F1_action"` ... `"F5_action"`}{`logical`
#'         columns for each action, ranging from `"F1_action"` to
#'         `"F5_action"` indicating if
#'         an action is associated with a project (`TRUE`) or not
#'         (`FALSE`).}
#'       \item{`"baseline_action"`}{`logical`
#'         column indicating if a project is associated with the baseline
#'         action (`TRUE`) or not (`FALSE`). This action is only
#'         associated with the baseline project.}
#'     }
#'   }
#'   \item{`sim_actions`}{A [tibble::tibble()] object containing
#'     data for six simulated actions. Each row corresponds to a
#'     different action and each column contains information about the
#'     actions. This table contains the following columns:
#'     \describe{
#'       \item{`"name"`}{`character` name for each action.}
#'       \item{`"cost"`}{`numeric` cost for each action.}
#'       \item{`"locked_in"`}{`logical` indicating if certain
#'         actions should be locked into the solution.}
#'       \item{`"locked_out"`}{`logical` indicating if certain
#'         actions should be locked out of the solution.}
#'     }
#'   }
#'   \item{`sim_features`}{A [tibble::tibble()] object containing
#'     data for five simulated features. Each row corresponds to a
#'     different feature and each column contains information about the
#'     features. This table contains the following columns:
#'     \describe{
#'       \item{`"name"`}{`character` name for each feature.}
#'       \item{`"weight"`}{`numeric` weight for each feature.}
#'     }
#'   }
#'   \item{tree}{[ape::phylo()] phylogenetic tree for the features.}
#' }
#'
#' @aliases sim_projects sim_actions sim_features sim_tree
#'
#' @keywords datasets
#'
#' @docType data
#'
#' @examples
#' # load data
#' data(sim_projects, sim_actions, sim_features, sim_tree)
#'
#' # print project data
#' print(sim_projects)
#
#' # print action data
#' print(sim_actions)
#'
#' # print feature data
#' print(sim_features)
#
#' # plot phylogenetic tree
#' plot(sim_tree)
#'
#' @name sim_data
NULL
