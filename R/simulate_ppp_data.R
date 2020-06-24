#' @include internal.R
NULL

#' Simulate data for the 'Project Prioritization Protocol'
#'
#' Simulate data for developing project prioritizations. Here, data are
#' simulated such that each feature has its own conservation project,
#' similar to species-based prioritizations (e.g. Bennett *et al.* 2014).
#'
#' @inheritParams simulate_ptm_data
#'
#' @details The simulated data set will contain one conservation project for
#'   each features, and also a "baseline" (do nothing) project to reflect
#'   features' persistence when their conservation project is not
#'   funded. Each conservation project is associated with a single action, and
#'   no conservation projects share any actions. Specifically, the data are
#'   simulated as follows:
#'
#'   \enumerate{
#'     \item A conservation project is created for each feature, and each
#'       project is associated with its own single action.
#'     \item Cost data for each action are simulated using a normal
#'       distribution and the `cost_mean` and `cost_sd` arguments.
#'     \item A set proportion of the actions are randomly set to be locked
#'       in and out of the solutions using the `locked_in_proportion` and
#'       `locked_out_proportion` arguments.
#'     \item The probability of each project succeeding if its action is funded
#'       is simulated by drawing probabilities from a uniform distribution with
#'       the upper and lower bounds set as the `success_min_probability`
#'       and `success_max_probability` arguments.
#'     \item The probability of each feature persisting if its project is funded
#'       and is successful is simulated by drawing probabilities from a uniform
#'       distribution with the upper and lower bounds set as the
#'       `funded_min_persistence_probability` and
#'       `funded_max_persistence_probability` arguments.
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
#'        \item{`"weight"`}{`numeric` weight for each feature.
#'          For each feature, this is calculated as the amount of time that
#'          elapsed between the present and the features' last common ancestor.
#'          In other words, the weights are calculated as the unique amount
#'          of evolutionary history that each feature has experienced.}
#'       }
#'     }
#'    \item{"tree"}{[ape::phylo()] phylogenetic tree for the
#'      features.}
#'  }
#'
#' @seealso [simulate_ptm_data()].
#'
#' @references
#' Bennett JR, Elliott G, Mellish B, Joseph LN, Tulloch AI,
#' Probert WJ, ... & Maloney R (2014) Balancing phylogenetic diversity
#' and species numbers in conservation prioritization, using a case study of
#' threatened species in New Zealand. *Biological Conservation*,
#' **174**: 47--54.
#'
#' @examples
#' # create a simulated data set
#' s <- simulate_ppp_data(number_features = 5,
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
simulate_ppp_data <- function(number_features, cost_mean = 100, cost_sd = 5,
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
    name = c(paste0("F", seq_len(number_features), "_action"),
             "baseline_action"),
    cost = c(stats::rnorm(number_features, cost_mean, cost_sd), 0),
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
    name = c(paste0("F", seq_len(number_features), "_project"),
                    "baseline_project"),
    success = c(stats::runif(number_features, success_min_probability,
                           success_max_probability), 1))

  ## feature persistence probabilities
  spp_prob_matrix <- matrix(NA_real_, ncol = number_features,
                            nrow = number_features + 1,
                            dimnames = list(NULL, tree$tip.label))
   diag(spp_prob_matrix) <- stats::runif(number_features,
                                         funded_min_persistence_probability,
                                         funded_max_persistence_probability)
  spp_prob_matrix[nrow(spp_prob_matrix), ] <-
    stats::runif(number_features, baseline_min_persistence_probability,
                 baseline_max_persistence_probability)
  projects <- cbind(projects, as.data.frame(spp_prob_matrix))

  ## organization data
  organization_data <- matrix(FALSE, ncol = number_features + 1,
                              nrow = number_features + 1,
                              dimnames = list(NULL, actions$name))
  diag(organization_data) <- TRUE
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
