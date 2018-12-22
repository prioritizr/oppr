#' @include internal.R pproto.R ProjectProblem-proto.R
NULL

#' Add relative targets
#'
#' Set targets as a proportion (between 0 and 1) of the maximum probability of
#' persistence associated with the best project for feature. For instance,
#' if the best project for a feature has an 80% probability of persisting,
#' setting a 50\% (i.e. \code{0.5}) relative target will correspond to a 40\%
#' threshold probability of persisting.
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param targets Object that specifies the targets for each feature. See the
#'   Details section for more information.
#'
#' @inherit add_absolute_targets details return seealso
#'
#' @examples
#' #TODO
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
  function(x, targets) standardGeneric("add_relative_targets"))

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
      max(targets) <= 1)
    # add targets
    add_manual_targets(x, tibble::tibble(feature = x$feature_names(),
                                         type = "relative",
                                         sense = ">=",
                                         target = targets))
})

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
      max(x$data$features[[targets]]) <= 1)
    # add targets to problem
    add_relative_targets(x, x$data$features[[targets]])
})
