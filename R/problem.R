#' @include internal.R ProjectProblem-proto.R
NULL

#' Project prioritization problem
#'
#' Create a project prioritization problem. This function is used to
#' specify the underlying data used in a prioritization problem: the projects,
#' the management actions, and the features that need
#' to be conserved (e.g. species, ecosystems). After constructing this
#' \code{ProjectProblem-class} object,
#' it can be customized using \code{\link{objectives}}, \code{\link{targets}},
#' \code{\link{weights}}, \code{\link{constraints}} and
#' \code{\link{solvers}}. After building the problem, the
#' \code{\link{solve}} function can be used to identify solutions.
#'
#' @param projects \code{\link[base]{data.frame}} or
#'   \code{\link[tibble]{tibble}} table containing project data. Here, each row
#'   should correspond to a different project and columns should contain data
#'   that correspond to each project. This object should contain data that
#'   denote (i)
#'   the name of each project (specified in the argument to
#'   \code{project_name_column}), (ii) the
#'   probability that each project will succeed if all of its actions are funded
#'   (specified in the argument to \code{project_success_column}), (iii)
#'   the enhanced probability that each feature will persist if it
#'   is funded (using a column for each feature),
#'   and (iv) and which actions are associated with which projects
#'   (using a column for each action). This object
#'   must have a baseline project, with a zero cost value, that represents the
#'   probability that each feature will persist if no other conservation
#'   project is funded.
#'   Since each feature is assigned the greatest probability of persistence
#'   given the funded projects in a solution, the combined benefits of multiple
#'   projects can be encoded by creating additional projects that represent
#'   "combined projects". For instance, a habitat restoration project might
#'   cost \$100 and mean that a feature has a 40\% chance of persisting, and
#'   a pest eradication project might cost \$50 and mean that a feature has a
#'   60\% chance of persisting. Due to non-linear effects, funding both of these
#'   projects might mean that a species has a 90\% chance of persistence.
#'   This can be accounted for by creating a third project, representing the
#'   funding of both projects, which costs \$150 and provides a 90\% chance
#'   of persistence.
#'
#' @param actions \code{\link[base]{data.frame}} or \code{\link[tibble]{tibble}}
#'   table containing the action data. Here, each row should correspond to a
#'   different action and columns should contain data that correspond to
#'   each action. At a minimum, this object should contain data that denote (i)
#'   the name of each action (specified in the argument to
#'   \code{action_name_column}), (ii) the cost of each action (specified in the
#'   argument to \code{action_cost_column}). Optionally, it may also contain
#'   data that indicate actions should be (iii) locked in or (iv) locked
#'   out (see \code{\link{add_locked_in_constraints}} and
#'   \code{\link{add_locked_out_constraints}}). It should also contain a
#'   zero-cost baseline action that is associated with the baseline project.
#
#'
#' @param features \code{\link[base]{data.frame}} or
#'   \code{\link[tibble]{tibble}}
#'   table containing the feature data. Here, each row should correspond
#'   to a different feature and columns should contain data that correspond
#'   to each feature. At a minimum, this object should contain data that denote
#'   (i) the name of each feature (specified in the argument to
#'   \code{feature_name_column}). Optionally, it may also contain (ii) the
#'   weight for each feature or (iii) persistence targets for each feature.
#'
#' @param project_name_column \code{character} name of column that contains
#'   the name for each conservation project. This argument corresponds to the
#'   \code{projects} table. Note that the project names must not contain any
#'   duplicates or missing values.
#'
#' @param project_success_column \code{character} name of column that
#'   indicates the probability that each project will succeed. This argument
#'   corresponds to the argument to \code{projects} table. This column must have
#'   \code{numeric} values which range between zero and one. No missing values
#'   are permitted.
#'
#' @param action_name_column \code{character} name of column that contains
#'   the name for each management action. This argument corresponds to the
#'   \code{actions} table. Note that the project names must not contain any
#'   duplicates or missing values.
#'
#' @param action_cost_column \code{character} name of column that
#'   indicates the cost for funding each action. This argument corresponds
#'   to the argument to \code{actions} table. This column must have
#'   \code{numeric} values which are equal to or greater than zero. No missing
#'   values are permitted.
#'
#' @param feature_name_column \code{character} name of the column that contains
#'   the name for each feature. This argument corresponds to the
#'   \code{feature} table. Note that the feature names must not contain any
#'   duplicates or missing values.
#'
#' @param adjust_for_baseline \code{logical} should the probability of
#'   features persisting when projects are funded be adjusted to account for the
#'   probability of features persisting under the baseline "do nothing"
#'   scenario in the event that the funded projects fail to succeed?
#'   This should always be \code{TRUE}, except when funding a project
#'   means that the baseline "do nothing" scenario does not apply if a funded
#'   project fails. Defaults to \code{TRUE}.
#'
#' @details
#'   A project prioritization problem has actions, projects,
#'   and features. Features are the biological entities that need to
#'   be conserved (e.g. species, populations, ecosystems). Actions are
#'   real-world management actions that can be implemented to enhance
#'   biodiversity (e.g. habitat restoration, monitoring, pest eradication). Each
#'   action should have a known cost, and this usually means that each
#'   action should have a defined spatial extent and time period (though this
#'   is not necessary). Conservation projects are groups of management actions
#'   (they can also comprise a singular action too), and each project is
#'   associated with a probability of success if all of its associated actions
#'   are funded. To determine which projects should be funded, each project is
#'   associated with an probability of persistence for the
#'   features that they benefit. These values should indicate the
#'   probability that each feature will persist if only that project funded
#'   and not the additional benefit relative to the baseline project. Missing
#'   (\code{NA}) values should be used to indicate which projects do not
#'   enhance the probability of certain features.
#'
#'   The goal of a project prioritization exercise is then to identify which
#'   management actions---and as a consequence which conservation
#'   projects---should be funded. Broadly speaking, the goal
#'   of an optimization problem is to minimize (or maximize) an objective
#'   function given a set of control variables and decision variables that are
#'   subject to a series of constraints. In the context of project
#'   prioritization problems, the
#'   objective is usually some measure of utility (e.g. the net
#'   probability of each feature persisting into the future), the
#'   control variables determine which actions should be funded or not,
#'   the decision variables contain additional information needed to
#'   ensure correct calculations,  and the
#'   constraints impose limits such as the total budget available for funding
#'   management actions. For more information on the mathematical
#'   formulations used in this package, please refer to the manual entries for
#'   the available objectives (listed in \code{\link{objectives}}).
#'
#' @return A fresh \code{\link{ProjectProblem-class}} object.
#'
#' @seealso \code{\link{constraints}}, \code{\link{decisions}},
#'  \code{\link{objectives}}, \code{\link{solvers}}, \code{\link{targets}},
#'  \code{\link{weights}}, \code{\link{solution_statistics}},
#'  \code{\link{plot.ProjectProblem}}.
#'
#' @name problem
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
#' @export
problem <- function(projects, actions, features, project_name_column,
                    project_success_column, action_name_column,
                    action_cost_column, feature_name_column,
                    adjust_for_baseline = TRUE) {
  # assertions
  ## coerce projects to tibble if just a regular data.frame
  if (inherits(projects, "data.frame") && !inherits(projects, "tbl_df"))
    projects <- tibble::as_tibble(projects)
  ## coerce actions to tibble if just a regular data.frame
  if (inherits(actions, "data.frame") && !inherits(actions, "tbl_df"))
    actions <- tibble::as_tibble(actions)
  ## coerce features to tibble if just a regular data.frame
  if (inherits(features, "data.frame") && !inherits(features, "tbl_df"))
    features <- tibble::as_tibble(features)
  ## assert that parameters are valid
  assertthat::assert_that(
    inherits(projects, "tbl_df"), ncol(projects) > 0, nrow(projects) > 0,
    inherits(actions, "tbl_df"), ncol(actions) > 0, nrow(actions) > 0,
    inherits(features, "tbl_df"), ncol(features) > 0, nrow(features) > 0,
    assertthat::is.string(project_name_column),
    assertthat::has_name(projects, project_name_column),
    assertthat::noNA(projects[[project_name_column]]),
    anyDuplicated(projects[[project_name_column]]) == 0,
    inherits(projects[[project_name_column]], c("character", "factor")),
    assertthat::is.string(project_success_column),
    assertthat::has_name(projects, project_success_column),
    is.numeric(projects[[project_success_column]]),
    assertthat::noNA(projects[[project_success_column]]),
    all(projects[[project_success_column]] >= 0),
    all(projects[[project_success_column]] <= 1),
    assertthat::is.string(action_name_column),
    assertthat::has_name(actions, action_name_column),
    assertthat::noNA(actions[[action_name_column]]),
    anyDuplicated(actions[[action_name_column]]) == 0,
    inherits(actions[[action_name_column]], c("character", "factor")),
    all(assertthat::has_name(projects, actions[[action_name_column]])),
    assertthat::noNA(unlist(projects[, actions[[action_name_column]]])),
    is.logical(as.matrix(projects[, actions[[action_name_column]]])),
    assertthat::is.string(action_cost_column),
    assertthat::has_name(actions, action_cost_column),
    is.numeric(actions[[action_cost_column]]),
    assertthat::noNA(actions[[action_cost_column]]),
    all(actions[[action_cost_column]] >= 0),
    assertthat::is.string(feature_name_column),
    assertthat::has_name(features, feature_name_column),
    assertthat::noNA(features[[feature_name_column]]),
    anyDuplicated(features[[feature_name_column]]) == 0,
    inherits(features[[feature_name_column]], c("character", "factor")),
    all(assertthat::has_name(projects, features[[feature_name_column]])),
    is.numeric(as.matrix(projects[, features[[feature_name_column]]])),
    min(as.matrix(projects[, features[[feature_name_column]]]),
        na.rm = TRUE) >= 0,
    max(as.matrix(projects[, features[[feature_name_column]]]),
        na.rm = TRUE) <= 1,
    assertthat::is.flag(adjust_for_baseline),
    assertthat::noNA(adjust_for_baseline))
  assertthat::assert_that(min(actions[[action_cost_column]]) == 0,
                          msg = "zero cost baseline project missing.")
  # verify that features have finite persistence probabilities in baseline
  # project(s)
  bp <- actions$name[actions[[action_cost_column]] == 0]
  assertthat::assert_that(length(bp) > 0,
    msg = "no baseline action detected (i.e. no projects have a zero cost)")
  assertthat::assert_that(length(bp) <= 1,
    msg = "multiple baseline actions detected")
  pa <- as.matrix(projects[, actions$name])
  bp <- which(vapply(seq_len(nrow(pa)), FUN.VALUE = logical(1), function(i) {
    setequal(actions$name[pa[i, ]], bp)
  }))
  assertthat::assert_that(length(bp) > 0, msg = "no baseline projects detected")
  bpp <- colSums(as.matrix(projects[bp, features[[feature_name_column]]]))
  assertthat::assert_that(all(is.finite(bpp)),
    msg = paste("feature(s) has a missing (NA) value for its",
                "probability of persistence under the baseline",
                "project, please provide baseline probabilities for:",
                paste(features[[feature_name_column]][!is.finite(bpp)],
                      collapse = ", "), "."))
  bpp <- colSums(as.matrix(projects[bp, features[[feature_name_column]]]),
                na.rm = TRUE)
  assertthat::assert_that(all(bpp > 1e-11),
    msg = paste("feature(s) has a zero probability of persistence under",
                "the baseline project, please replace these zeros with",
                "a small number (e.g. 1e-10) for:",
                paste(features[[feature_name_column]][bpp <= 1e-11],
                      collapse = ", "), "."))
  # create ProjectProblem object
  pproto(NULL, ProjectProblem,
         constraints = pproto(NULL, Collection),
         data = list(projects = projects, actions = actions,
                     features = features,
                     project_name_column = project_name_column,
                     project_success_column = project_success_column,
                     action_name_column = action_name_column,
                     action_cost_column = action_cost_column,
                     feature_name_column = feature_name_column,
                     adjust_for_baseline = adjust_for_baseline))
}
