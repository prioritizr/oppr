#' @include internal.R
NULL

#' Simulate data for 'Priority threat management'
#'
#' Simulate data for developing project prioritizations for a priority threat
#' management exercise (Carwardine *et al.* 2019). Here, data are
#' simulated for a pre-specified number of features, actions, and projects.
#' Features can benefit from multiple projects, and different projects can
#' share actions.
#'
#' @param number_projects `numeric` number of projects. Note that this
#'   does not include the baseline project.
#'
#' @param number_actions `numeric` number of actions. Note that this
#'   does not include the baseline action.
#'
#' @param number_features `numeric` number of features.
#'
#' @param cost_mean `numeric` average cost for the actions. Defaults to
#'   `100`.
#'
#' @param cost_sd `numeric` standard deviation in action costs. Defaults
#'   to `5`.
#'
#' @param success_min_probability `numeric` minimum probability of the
#'   projects succeeding if they are funded. Defaults to `0.7`.
#'
#' @param success_max_probability `numeric` maximum probability of the
#'   projects succeeding if they are funded. Defaults to `0.99`.
#'
#' @param funded_min_persistence_probability `numeric` minimum probability
#'   of the features persisting if projects are funded and successful.
#'   Defaults to `0.5`.
#'
#' @param funded_max_persistence_probability `numeric` maximum probability
#'   of the features persisting if projects are funded and successful.
#'   Defaults to `0.9`.
#'
#' @param baseline_min_persistence_probability `numeric` minimum
#'   probability of the features persisting if only the baseline project
#'   is funded. Defaults to `0.01`.
#'
#' @param baseline_max_persistence_probability `numeric` maximum
#'   probability of the features persisting if only the baseline project is
#'   funded. Defaults to `0.4`.
#'
#' @param locked_in_proportion `numeric` of actions that are locked
#'   into the solution. Defaults to `0`.
#'
#' @param locked_out_proportion `numeric` of actions that are locked
#'   into the solution. Defaults to `0`.
#'
#' @details The simulated data set will contain one conservation project for
#'   each features, and also a "baseline" (do nothing) project to reflect
#'   features' persistence when their conservation project is not
#'   funded. Each conservation project is associated with a single action, and
#'   no conservation projects share any actions. Specifically, the data are
#'   simulated as follows:
#'
#'   \enumerate{
#'     \item A specified number of conservation projects, features, and
#'       management actions are created.
#'     \item Cost data for each action are simulated using a normal
#'       distribution and the `cost_mean` and `cost_sd` arguments.
#'     \item A set proportion of the actions are randomly set to be locked
#'       in and out of the solutions using the `locked_in_proportion` and
#'       `locked_out_proportion` arguments.
#'     \item The probability of each project succeeding if its action is funded
#'       is simulated by drawing probabilities from a uniform distribution with
#'       the upper and lower bounds set as the `success_min_probability`
#'       and `success_max_probability` arguments.
#'     \item The probability of each feature persisting if various projects
#'       are funded and is successful is simulated by drawing probabilities
#'       from a uniform distribution with the upper and lower bounds set as the
#'       `funded_min_persistence_probability` and
#'       `funded_max_persistence_probability` arguments. To prevent
#'     \item An additional project is created which represents the "baseline"
#'       (do nothing) scenario. The probability of each feature persisting
#'       when managed under this project is simulated by drawing probabilities
#'       from a uniform distribution with the upper and lower bounds
#'       set as the `baseline_min_persistence_probability`
#'       and `baseline_max_persistence_probability` arguments.
#'     \item A phylogenetic tree is simulated for the features using
#'       [ape::rcoal()].
#'     \item Feature data are created from the phylogenetic tree. The
#'       weights are calculated as the amount of evolutionary history
#'       that has elapsed between each feature and its last common ancestor.
#'  }
#'
#' @return A `list` object containing the elements:
#'
#'   \describe{
#'     \item{`"projects"`}{A [tibble::tibble()] containing
#'       the data for the conservation projects. It contains the following
#'       columns:
#'       \describe{
#'         \item{`"name"`}{`character` name for each project.}
#'         \item{`"success"`}{`numeric` probability of each project
#'           succeeding if it is funded.}
#'         \item{`"F1"` ... `"FN"`}{`numeric` columns for each
#'           feature, ranging from `"F1"` to `"FN"` where `N`
#'           is the number of features, indicating the enhanced probability that
#'           each feature will persist if it funded. Missing values (`NA`)
#'           indicate that a feature does not benefit from a project being
#'           funded.}
#'         \item{`"F1_action"` ... `"FN_action"`}{`logical`
#'           columns for each action, ranging from `"F1_action"` to
#'           `"FN_action"` where `N` is
#'           the number of actions (equal to the number of features in this
#'           simulated data), indicating if an action is associated with a
#'           project (`TRUE`) or not (`FALSE`).}
#'        \item{`"baseline_action"`}{`logical`
#'          column indicating if a project is associated with the baseline
#'          action (`TRUE`) or not (`FALSE`). This action is only
#'          associated with the baseline project.}
#'        }
#'     }
#'     \item{`"actions"`}{A [tibble::tibble()] containing
#'       the data for the conservation actions. It contains the following
#'       columns:
#'       \describe{
#'         \item{`"name"`}{`character` name for each action.}
#'         \item{`"cost"`}{`numeric` cost for each action.}
#'         \item{`"locked_in"`}{`logical` indicating if certain
#'           actions should be locked into the solution.}
#'         \item{`"locked_out"`}{`logical` indicating if certain
#'           actions should be locked out of the solution.}
#'       }
#'     }
#'     \item{`"features"`}{A [tibble::tibble()] containing
#'       the data for the conservation features (e.g. species). It contains the
#'       following columns:
#'       \describe{
#'        \item{`"name"`}{`character` name for each feature.}
#'         \item{`"weight"`}{`numeric` weight for each feature.
#'           For each feature, this is calculated as the amount of time that
#'           elapsed between the present and the features' last common ancestor.
#'           In other words, the weights are calculated as the unique amount
#'           of evolutionary history that each feature has experienced.}
#'         }
#'     }
#'    \item{"tree"}{[ape::phylo()] phylogenetic tree for the
#'      features.}
#'  }
#'
#' @seealso [simulate_ppp_data()].
#'
#' @references
#' Carwardine J, Martin TG, Firn J, Ponce-Reyes P, Nicol S, Reeson A,
#' Grantham HS, Stratford D, Kehoe L, Chades I (2019) Priority Threat
#' Management for biodiversity conservation: A handbook.
#' *Journal of Applied Ecology*, **56**: 481--490.
#'
#' @examples
#' # create a simulated data set
#' s <- simulate_ptm_data(number_projects = 6,
#'                        number_actions = 8,
#'                        number_features = 5,
#'                        cost_mean = 100,
#'                        cost_sd = 5,
#'                        success_min_probability = 0.7,
#'                        success_max_probability = 0.99,
#'                        funded_min_persistence_probability = 0.5,
#'                        funded_max_persistence_probability = 0.9,
#'                        baseline_min_persistence_probability = 0.01,
#'                        baseline_max_persistence_probability = 0.4,
#'                        locked_in_proportion = 0.01,
#'                        locked_out_proportion = 0.01)
#'
#' # print data set
#' print(s)
#'
#' @export
simulate_ptm_data <- function(number_projects, number_actions, number_features,
                              cost_mean = 100, cost_sd = 5,
                              success_min_probability = 0.7,
                              success_max_probability = 0.99,
                              funded_min_persistence_probability = 0.5,
                              funded_max_persistence_probability = 0.9,
                              baseline_min_persistence_probability = 0.01,
                              baseline_max_persistence_probability = 0.4,
                              locked_in_proportion = 0,
                              locked_out_proportion = 0) {
  # assert that arguments are valid
  assertthat::assert_that(
    assertthat::is.count(number_features),
    isTRUE(is.finite(number_features)),
    assertthat::is.count(number_projects),
    isTRUE(is.finite(number_projects)),
    assertthat::is.count(number_actions),
    isTRUE(is.finite(number_actions)),
    assertthat::is.number(cost_mean),
    isTRUE(cost_mean > 0),
    assertthat::is.number(cost_sd),
    isTRUE(cost_sd > 0),
    assertthat::is.number(success_min_probability),
    isTRUE(success_min_probability >= 0),
    isTRUE(success_min_probability <= 1),
    assertthat::is.number(success_max_probability),
    isTRUE(success_max_probability >= 0),
    isTRUE(success_max_probability <= 1),
    isTRUE(success_max_probability > success_min_probability),
    assertthat::is.number(funded_min_persistence_probability),
    isTRUE(funded_min_persistence_probability >= 0),
    isTRUE(funded_min_persistence_probability <= 1),
    assertthat::is.number(funded_max_persistence_probability),
    isTRUE(funded_max_persistence_probability >= 0),
    isTRUE(funded_max_persistence_probability <= 1),
    isTRUE(funded_max_persistence_probability >
           funded_min_persistence_probability),
    assertthat::is.number(baseline_min_persistence_probability),
    isTRUE(baseline_min_persistence_probability >= 0),
    isTRUE(baseline_min_persistence_probability <= 1),
    assertthat::is.number(baseline_max_persistence_probability),
    isTRUE(baseline_max_persistence_probability >= 0),
    isTRUE(baseline_max_persistence_probability <= 1),
    isTRUE(baseline_max_persistence_probability >
           baseline_min_persistence_probability),
    isTRUE(funded_min_persistence_probability >
           baseline_max_persistence_probability),
    assertthat::is.number(locked_in_proportion),
    isTRUE(locked_in_proportion >= 0),
    isTRUE(locked_in_proportion <= 1),
    assertthat::is.number(locked_out_proportion),
    isTRUE(locked_out_proportion >= 0),
    isTRUE(locked_out_proportion <= 1))
    assertthat::assert_that(
      isTRUE(number_features >
             (ceiling(number_features * locked_in_proportion) +
             ceiling(number_features * locked_out_proportion))),
      msg = paste("combined number of locked in and locked out projects",
                  "exceeds the total number of projects."))

  # create action data
  actions <- tibble::tibble(
    name = c(paste0("action_", seq_len(number_actions)),
             "baseline_action"),
    cost = c(stats::rnorm(number_actions, cost_mean, cost_sd), 0),
    locked_in = FALSE,
    locked_out = FALSE)
  assertthat::assert_that(all(actions$cost >= 0),
  msg = paste("some projects have subzero costs, increase the argument to",
              "cost_mean and try again"))

  # assign locked in actions
  if (locked_in_proportion > 1e-10) {
    l <- sample.int(number_features, ceiling(number_features *
                                            locked_in_proportion))
    actions$locked_in[l] <- TRUE
  }

  # assign locked out actions
  if (locked_out_proportion > 1e-10) {
    l <- sample(which(!actions$locked_in &
                      seq_len(nrow(actions)) != nrow(actions)),
                ceiling(number_features * locked_out_proportion))
    actions$locked_out[l] <- TRUE
  }

  # phylogenetic tree
  tree <- ape::rcoal(n = number_features)
  tree$tip.label <- paste0("F", seq_len(number_features))

  # create project data
  projects <- tibble::tibble(
    name = c(paste0("project_", seq_len(number_projects)), "baseline_project"),
    success = c(stats::runif(number_projects, success_min_probability,
                           success_max_probability), 1))

  ## feature persistence probabilities
  ### initialize with probabilities in every project/feature combination
  spp_prob_matrix <- matrix(
    stats::runif(number_features * (number_projects + 1),
                 funded_min_persistence_probability,
                 funded_max_persistence_probability),
    ncol = number_features, nrow = number_projects + 1,
    dimnames = list(NULL, tree$tip.label))
  ### randomly introduce sparsity (NAs) into the matrix, so that not every
  ### feature benefits from every project
  for (i in seq_len(nrow(spp_prob_matrix) - 1)) {
    spp_prob_matrix[i, sample(seq_len(number_features),
                              sample.int(number_features - 1, 1))] <- NA_real_
  }
  ### assign probabilities for baseline project
  spp_prob_matrix[nrow(spp_prob_matrix), ] <-
    stats::runif(number_features, baseline_min_persistence_probability,
                 baseline_max_persistence_probability)
  projects <- cbind(projects, as.data.frame(spp_prob_matrix))

  ## organization data
  organization_data <- matrix(FALSE, ncol = number_actions + 1,
                              nrow = number_projects + 1,
                              dimnames = list(NULL, actions$name))
  for (i in seq_len(number_projects)) {
    organization_data[i, sample(seq_len(number_actions),
                             sample.int(number_actions, 1))] <- TRUE
  }
  organization_data[number_projects + 1, number_actions + 1] <- TRUE
  projects <- cbind(projects, as.data.frame(organization_data))

  ## feature data
  features <- tibble::tibble(
    name = tree$tip.label,
    weight = tree$edge.length[match(seq_along(tree$tip.label),
                                    tree$edge[, 2])])

  ## return result
  list(projects = tibble::as_tibble(projects),
       actions = actions,
       features = features,
       tree = tree)
}
