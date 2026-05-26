#' @include internal.R
NULL

#' Simulate multi-objective data for the 'Project Prioritization Protocol'
#'
#' Simulate data for developing multi-objective project prioritizations.
#' Here, data are simulated such that each objective has its own features,
#' and each feature has its own conservation project.
#' This structure is similar to species-based prioritizations
#' (e.g., Bennett *et al.* 2014).
#'
#' @inheritParams simulate_ptm_data
#'
#' @param number_objectives Number of objectives for which to simulate data.
#' @param number_actions Number of actions for which to simulate data.
#'
#' @details
#' The simulated data set will contain a set of projects,
#' wherein each project is assigned to a particular objective and
#' is associated with a particular feature.
#' Although multiple projects may be assigned to the same objective,
#' note that each project is associated with a different feature.
#' Also note that that each objective is associated with a "baseline"
#' "baseline" (do nothing) project to reflect
#' features' persistence when their conservation project is not
#' funded. Specifically, the data are simulated using the following procedure.
#'
#' \enumerate{
#'
#' \item A set of objectives are defined (per `number_objectives`)
#' and a set of features are defined (per `number_features`).
#'
#' \item Each feature is then randomly assigned to each objective.
#' Note that each objective will always have at least one of feature.
#'
#' \item A set of actions (per `number_actions`) are simulated and the costs
#' for these actions are simulated using a normal distribution and the
#' `cost_mean` and `cost_sd` arguments. In addition to these actions,
#' a set of additional baseline actions are simulated for each objective.
#'
#' \item A set of projects are created for each feature by randomly
#' selecting a set of actions.
#'
#' \item A set proportion of the actions are randomly set to be locked
#' in and out of the solutions using the `locked_in_proportion` and
#' `locked_out_proportion` arguments.
#'
#' \item The probability of each project succeeding if its action is funded
#' is simulated by drawing probabilities from a uniform distribution with
#' the upper and lower bounds set as the `success_min_probability`
#' and `success_max_probability` arguments.
#'
#' \item The probability of each feature persisting if its project is funded
#' and is successful is simulated by drawing probabilities from a uniform
#' distribution with the upper and lower bounds set as the
#' `funded_min_persistence_probability` and
#' `funded_max_persistence_probability` arguments.
#'
#' \item An additional project is created for each programme which represents
#' the "baseline" (do nothing) scenario.
#' The probability of each feature persisting
#' when managed under this project is simulated by drawing probabilities
#' from a uniform distribution with the upper and lower bounds
#' set as the `baseline_min_persistence_probability`
#' and `baseline_max_persistence_probability` arguments.
#'
#' \item A phylogenetic tree is simulated for the features that belong
#' to each objective (separately) using
#' [ape::rcoal()].
#'
#' \item Feature data are created from the phylogenetic tree. The
#' weights are calculated as the amount of evolutionary history
#' that has elapsed between each feature and its last common ancestor.
#'
#' }
#'
#' @return
#' A `list` object containing the following elements.
#'
#' \describe{
#'
#' \item{`"projects_obj1"`, ..., `"projects_objN"`}{
#' A `list` of [tibble::tibble()] containing
#' data for the conservation projects associated with each objective.
#' Each element contains a data frame with the following following
#' columns.
#'
#' \describe{
#'
#' \item{`"name"`}{
#' `character` name for each project.
#' }
#'
#' \item{`"success"`}{
#' `numeric` probability of each project succeeding if it is funded.
#' }
#'
#' \item{`"F1"` ... `"FN"`}{
#' `numeric` columns for each
#' feature, ranging from `"F1"` to `"FN"` where `N`
#' is the number of features, indicating the probability that
#' each feature will persist if it is funded and successfully completed.
#' Missing values (`NA`)
#' indicate that a feature does not benefit from a project being
#' funded.
#' }
#'
#' \item{`"F1_action"` ... `"FN_action"`}{
#' `logical` columns for each action, ranging from `"F1_action"` to
#' `"FN_action"` where `N` is
#' the number of actions (equal to the number of features in this
#' simulated data), indicating if an action is associated with a
#' project (`TRUE`) or not (`FALSE`).
#' }
#'
#' \item{`"baseline_action"`}{
#' `logical` column indicating if a project is associated with the baseline
#' action (`TRUE`) or not (`FALSE`). This action is only
#' associated with the baseline project.
#' }
#
#' }
#' }
#'
#' \item{`"actions"`}{
#' A [tibble::tibble()] containing
#' the data for the conservation actions across all objectives.
#' It contains the following columns.
#'
#' \describe{
#'
#' \item{`"name"`}{
#' `character` name for each action.
#' }
#'
#' \item{`"cost"`}{
#' `numeric` cost for each action.
#' }
#'
#' \item{`"locked_in"`}{
#' `logical` indicating if certain actions should be locked into the solution.
#' }
#'
#' \item{`"locked_out"`}{
#' `logical` indicating if certain actions should be locked out of the
#' solution.
#' }
#'
#' }
#' }
#'
#' \item{`"features_obj1"`, ..., `"features_objN"`}{
#' A `list` of [tibble::tibble()] containing
#' data for the features (e.g., species) associated with each objective.
#' Each element contains a data frame with the following following
#' columns.
#'
#' \describe{
#'
#' \item{`"name"`}{
#' `character` name for each feature.
#' }
#'
#' \item{`"weight"`}{
#' `numeric` weight for each feature.
#' For each feature, this is calculated as the amount of time that
#' elapsed between the present and the features' last common ancestor.
#' In other words, the weights are calculated as the unique amount
#' of evolutionary history that each feature has experienced.
#' }
#'
#' }
#' }
#'
#' \item{`"tree_obj1"`, ..., `"tree_objN"`}{
#' A `list` of [ape::phylo()] phylogenetic tree objects for the features
#' associated with each objective (separately).
#' }
#'
#' }
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
#' s <- simulate_multi_ppp_data(
#'   number_objectives = 3,
#'   number_features = 7,
#'   number_actions = 5,
#'   cost_mean = 100,
#'   cost_sd = 5,
#'   success_min_probability = 0.7,
#'   success_max_probability = 0.99,
#'   funded_min_persistence_probability = 0.5,
#'   funded_max_persistence_probability = 0.9,
#'   baseline_min_persistence_probability = 0.01,
#'   baseline_max_persistence_probability = 0.4,
#'   locked_in_proportion = 0.01,
#'   locked_out_proportion = 0.01
#' )
#'
#' # print data set
#' print(s)
#'
#' @export
simulate_multi_ppp_data <- function(number_objectives,
                                    number_features,
                                    number_actions,
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
    assertthat::is.count(number_objectives),
    isTRUE(is.finite(number_objectives)),
    assertthat::is.count(number_features),
    isTRUE(is.finite(number_features)),
    assertthat::is.count(number_actions),
    isTRUE(is.finite(number_actions)),
    number_features >= (number_objectives * 2),
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
    isTRUE(locked_out_proportion <= 1)
  )
  assertthat::assert_that(
    isTRUE(number_features >
      (ceiling(number_features * locked_in_proportion) +
        ceiling(number_features * locked_out_proportion))),
    msg = paste(
      "combined number of locked in and locked out projects",
      "exceeds the total number of projects."
    )
  )

  # create action data
  actions <- tibble::tibble(
    name = c(
      paste0("A", seq_len(number_actions), "_action"),
      paste0("B", seq_len(number_objectives), "_action")
    ),
    cost = c(
      stats::rnorm(number_actions, cost_mean, cost_sd),
      rep(0, number_objectives)
    ),
    locked_in = FALSE,
    locked_out = FALSE
  )
  assertthat::assert_that(
    all(actions$cost >= 0),
    msg = paste(
      "some projects have subzero costs, increase the argument to",
      "cost_mean and try again"
    )
  )

  # assign locked in actions
  if (locked_in_proportion > 1e-10) {
    l <- sample.int(
      number_actions, ceiling(number_actions * locked_in_proportion)
    )
    actions$locked_in[l] <- TRUE
  }

  # assign locked out actions
  if (locked_out_proportion > 1e-10) {
    l <- sample(
      which(!actions$locked_in & seq_len(nrow(actions)) != nrow(actions)),
      ceiling(number_actions * locked_out_proportion)
    )
    actions$locked_out[l] <- TRUE
  }

  # simulate internal feature data and assign them to objectives
  feature_data <- tibble::tibble(
    name = paste0("F", seq_len(number_features)),
    objective = sample(
      paste0("O", seq_len(number_objectives)),
      number_features,
      replace = TRUE
    )
  )
  feature_data$objective[seq_len(number_objectives * 2)] <-
    rep(paste0("O", seq_len(number_objectives)), each = 2)

  # phylogenetic tree
  tree <- lapply(
    seq_len(number_objectives), function(i) {
      idx <- feature_data$objective == paste0("O", i)
      d <- ape::rcoal(n = sum(idx))
      d$tip.label <- feature_data$name[idx]
      d
    }
  )
  names(tree) <- paste0("tree_obj", seq_len(number_objectives))

  # create project data
  projects <- lapply(seq_len(number_objectives), function(i) {
    # identify features in objective
    idx <- feature_data$objective == paste0("O", i)
    # initialize data
    d <- feature_data[idx, "name", drop = FALSE]
    # set project names
    d$name <- paste0(d$name, "_project")
    # simulate probability success data
    d$success = stats::runif(
      nrow(d), success_min_probability, success_max_probability
    )
    # simulate baseline project data
    d <- rbind(
      d,
      tibble::tibble(
        name = paste0("baseline_project_obj", i),
        success = 1
      )
    )
    # simulate feature persistence probabilities
    feature_prob_matrix <- matrix(
      NA_real_,
      ncol = sum(idx),
      nrow = sum(idx) + 1,
      dimnames = list(NULL, feature_data$name[idx])
    )
    diag(feature_prob_matrix) <- stats::runif(
      sum(idx),
      funded_min_persistence_probability,
      funded_max_persistence_probability
    )
    feature_prob_matrix[nrow(feature_prob_matrix), ] <- stats::runif(
      sum(idx),
      baseline_min_persistence_probability,
      baseline_max_persistence_probability
    )
    # merge columns
    d <- cbind(d, as.data.frame(feature_prob_matrix))
    # simulate data to specify which actions are associated with each project
    org_matrix <- matrix(
      FALSE,
      ncol = length(actions$name),
      nrow = sum(idx) + 1,
      dimnames = list(NULL, actions$name)
    )
    ## non-baseline projects
    for (j in seq_len(sum(idx))) {
      curr_n <- sample.int(number_actions, 1)
      curr_idx <- sample.int(number_actions, curr_n)
      org_matrix[j, curr_idx] <- TRUE
    }
    ## baseline project
    org_matrix[
      nrow(org_matrix),
      which(actions$name == paste0("B", i, "_action"))
    ] <- TRUE
    # merge columns
    d <- cbind(d, as.data.frame(org_matrix))
    # output as tibble
    tibble::as_tibble(d)
  })
  names(projects) <- paste0("projects_obj", seq_len(number_objectives))

  # feature data
  features <- lapply(seq_len(number_objectives), function(i) {
    # identify features in objective
    idx <- feature_data$objective == paste0("O", i)
    # initialize data
    d <- feature_data[idx, "name", drop = FALSE]
    # compute phylogenetic weight from free
    d$weight <- tree[[i]]$edge.length[
      match(seq_along(tree[[i]]$tip.label), tree[[i]]$edge[, 2])
    ]
    # return result
    d
  })

  # return result
  list(
    projects = projects,
    actions = actions,
    features = features,
    tree = tree
  )
}
