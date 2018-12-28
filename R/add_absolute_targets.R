#' @include internal.R pproto.R ProjectProblem-proto.R
NULL

#' Add absolute targets
#'
#' Set targets for a project prioritization \code{\link{problem}} by
#' specifying exactly what probability of persistence is required
#' for each feature. For instance, setting an absolute target of 10\%
#' (i.e. \code{0.1}) corresponds to a threshold 10\% probability of persisting.
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param targets Object that specifies the targets for each feature. See the
#'   Details section for more information.
#'
#' @details Targets are used to specify the minimum probability of persistence
#'   for each feature in solutions. For minimum set objectives
#'   (i.e. \code{\link{add_min_set_objective}}, these targets
#'   specify the minimum probability of persistence required for each species
#'   in the solution. And for budget constrained objectives that use targets
#'   (i.e. \code{\link{add_max_target_phylo_objective}} and
#'   \code{\link{add_max_target_richness_objective}}), these targets
#'   specify the minimum threshold probability of persistence that needs to be
#'   achieved to count the benefits for conserving these species.
#'   Please note that attempting to solve problems with objectives that require
#'   targets without specifying targets will throw an error.
#'
#'   The targets for a problem can be specified in several different ways:
#'
#'   \describe{
#'
#'   \item{\code{numeric}}{\code{vector} of target values for each feature.
#'     Additionally, for convenience, this type of argument can be a single
#'     value to assign the same target to each feature.}
#'
#'   \item{\code{character}}{specifying the name of column in the
#'     feature data (i.e. the argument to \code{features} in the
#'     \code{\link{problem}} function) that contains the persistence targets.}
#'
#'   }
#'
#' @seealso \code{\link{targets}}.
#'
#' @examples
#' #TODO
#'
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
  function(x, targets) standardGeneric("add_absolute_targets"))

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
      max(targets) <= 1)
    # add targets
    add_manual_targets(x, tibble::tibble(feature = x$feature_names(),
                                         type = "absolute",
                                         sense = ">=",
                                         target = targets))
})

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
      max(x$data$features[[targets]]) <= 1)
    # add targets to problem
    add_absolute_targets(x, x$data$features[[targets]])
})
