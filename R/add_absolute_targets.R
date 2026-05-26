#' @include internal.R ProjectProblem-class.R
NULL

#' Add absolute targets
#'
#' Add targets to a project prioritization that specify the
#' desired expected outcome for each feature in the same units
#' as the outcomes values.
#' For example, if a feature has its outcome values expressed
#' probabilities of persistence, then setting an absolute target of 0.1
#' means that the feature should ideally have a 10% chance of persistence.
#'
#' @inheritParams add_manual_targets
#'
#' @param targets Object that specifies the targets for each feature. See the
#' Details section for more information.
#'
#' @details
#' Targets are used to specify a threshold minimum desirable
#' expected outcome for each feature. These should ideally be set
#' according to stakeholder requirements and expert knowledge.
#' Please note that attempting to solve problems with objectives that require
#' targets without specifying targets will throw an error.
#'
#' The targets for a problem can be specified using the following options.
#'
#' \describe{
#'
#' \item{`numeric` value}{
#' The value is used to set the target threshold for each feature.
#' This option may be useful when all features should be assigned the same
#' target threshold.
#' }
#'
#' \item{`numeric` vector}{
#' Each value specifies a target threshold for each feature.
#' The order of the values should correspond to the order
#' of the features in `x`.
#' }
#'
#' \item{`character` value}{
#' The value specifies the name of a column in the
#' feature data (i.e., the argument to `features` in the
#' [problem()] function). The target threshold for each feature
#' is set according the column values.
#' }
#'
#' }
#'
#' @inherit add_manual_targets return seealso
#'
#' @family targets
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with minimum set objective and targets that require each
#' # feature to have a 30% chance of persisting into the future
#' p1 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(0.3) %>%
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
#'   add_absolute_targets(c(0.1, 0.2, 0.3, 0.4, 0.5)) %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p2)
#'
#' # add a column name to the feature data with targets
#' sim_features$target <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#'
#' # build problem with minimum set objective and specify targets using
#' # column name in the feature data
#' p3 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets("target") %>%
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
#' @aliases add_absolute_targets,ProjectProblem,numeric-method add_absolute_targets,ProjectProblem,character-method
#'
#' @name add_absolute_targets
#'
NULL

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @exportMethod add_absolute_targets
#' @export
methods::setGeneric(
  "add_absolute_targets",
  signature = methods::signature("x", "targets"),
  function(x, targets) standardGeneric("add_absolute_targets")
)

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage \S4method{add_absolute_targets}{ProjectProblem,numeric}(x, targets)
methods::setMethod(
  "add_absolute_targets",
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
        type = "absolute",
        sense = ">=",
        target = targets
      )
    )
  }
)

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage \S4method{add_absolute_targets}{ProjectProblem,character}(x, targets)
methods::setMethod(
  "add_absolute_targets",
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
    add_absolute_targets(x, x$data$features[[targets]])
  }
)
