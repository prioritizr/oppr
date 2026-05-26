#' @include internal.R ProjectProblem-class.R
NULL

#' Add relative targets
#'
#' Add targets to a project prioritization problem that specify the
#' desired expected outcome for each feature as a proportion of the
#' the best possible outcome that could be achieved.
#' For instance, if a feature is associated with three projects that
#' would be expected to result in a 30%, 60%, and 80% chance of persistence
#' (adjusted for baseline outcomes), then setting a relative target of 0.5
#' would correspond to a 40% chance of persistence (i.e., 50% times 80%).
#'
#' @inheritParams add_manual_targets
#'
#' @param targets Object that specifies the targets for each feature. See the
#' Details section for more information.
#'
#' @inherit add_absolute_targets details return seealso
#'
#' @family targets
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with minimum set objective and targets that require each
#' # feature to have a level of persistence that is greater than or equal to
#' # 70% of the best project for conserving it
#' p1 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.7) %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p1)
#'
#' # build problem with minimum set objective and specify targets that require
#' # different levels of persistence for each feature
#' p2 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(c(0.2, 0.3, 0.4, 0.5, 0.6)) %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p2)
#'
#' # add a column name to the feature data with targets
#' sim_features$target <- c(0.2, 0.3, 0.4, 0.5, 0.6)
#'
#' # build problem with minimum set objective and specify targets using
#' # column name in the feature data
#' p3 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets("target") %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p3)
#'
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#'
#' # print solutions
#' print(s1)
#' print(s2)
#' print(s3)
#'
#' # plot solutions
#' plot(p1, s1)
#' plot(p2, s2)
#' plot(p3, s3)
#' @aliases add_relative_targets,ProjectProblem,numeric-method add_relative_targets,ProjectProblem,character-method
#'
#' @name add_relative_targets
NULL

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @exportMethod add_relative_targets
#' @export
methods::setGeneric(
  "add_relative_targets",
  signature = methods::signature("x", "targets"),
  function(x, targets) standardGeneric("add_relative_targets")
)

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ProjectProblem,numeric}(x, targets)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ProjectProblem", "numeric"),
  function(x, targets) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      length(targets) %in% c(1, number_of_features(x)),
      is.numeric(targets),
      assertthat::noNA(targets),
      min(targets) >= 0,
      max(targets) <= 1
    )
    # add targets
    add_manual_targets(
      x,
      tibble::tibble(
        feature = x$feature_names(),
        type = "relative",
        sense = ">=",
        target = targets
      )
    )
  }
)

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ProjectProblem,character}(x, targets)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ProjectProblem", "character"),
  function(x, targets) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      assertthat::is.string(targets),
      assertthat::noNA(targets),
      assertthat::has_name(x$data$features, targets),
      is.numeric(x$data$features[[targets]]),
      assertthat::noNA(x$data$features[[targets]]),
      min(x$data$features[[targets]]) >= 0,
      max(x$data$features[[targets]]) <= 1
    )
    # add targets to problem
    add_relative_targets(x, x$data$features[[targets]])
  }
)
