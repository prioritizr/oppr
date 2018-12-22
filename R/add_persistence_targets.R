 #' @include internal.R pproto.R ProjectProblem-proto.R
NULL

#' Add persistence targets
#'
#' Set persistence targets for a project prioritization \code{\link{problem}}.
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
#'   \item{\code{character}}{containing the names of columns in the
#'     feature data (i.e. the argument to \code{features} in the
#'     \code{\link{problem}} function)that contain persistence targets.}
#'
#'   }
#'
#' @examples
#' #TODO
#'
#' @name add_persistence_targets
#'
NULL

#' @name add_persistence_targets
#' @rdname add_persistence_targets
#' @exportMethod add_persistence_targets
#' @export
methods::setGeneric(
  "add_persistence_targets",
  signature = methods::signature("x", "targets"),
  function(x, targets) standardGeneric("add_persistence_targets"))

#' @name add_persistence_targets
#' @rdname add_persistence_targets
#' @usage \S4method{add_persistence_targets}{ProjectProblem,numeric}(x, targets)
methods::setMethod(
  "add_persistence_targets",
  methods::signature("ProjectProblem", "numeric"),
  function(x, targets) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      length(targets) %in% c(1, number_of_features(x)),
      is.numeric(x$data$features[[targets]]),
      assertthat::noNA(x$data$features[[targets]]),
      min(x$data$features[[targets]]) >= 0,
      max(x$data$features[[targets]]) <= 1)
    # create targets
   target_data <- tibble::tibble(feature = x$feature_names(), sense = ">=",
                                 value = targets)
    # define function to validate changes to the targets object
    vfun <- function(x) !inherits(try(validate_targets(x), silent = TRUE),
                                  "try-error")
    # define function to render targets object
    rfun <- function(x)
      utils::getFromNamespace("rHandsontableOutput", "rhandsontable")(x)
    # add targets to problem
    x$add_targets(pproto(
      "PersistenceTargets",
      Target,
      name = "Targets",
      parameters = parameters(misc_parameter("Targets", target_data, vfun,
                                             rfun)),
      data = list(feature_names = x$feature_names()),
      repr = function(self) {
        targets <- self$parameters$get("Targets")
        out <- paste0(out, " targets [targets (min: ", min(targets$target),
                      ", max: ", max(targets$target), ")]")
        return(out)
       },
       output = function(self) {
         # get data
         targets <- self$parameters$get("Targets")
         # convert feature names to indices
         targets$feature <- match(targets$feature, feature_names)
         # return tibble
         targets
       }
    ))
})

#' @name add_persistence_targets
#' @rdname add_persistence_targets
#' @usage \S4method{add_persistence_targets}{ProjectProblem,character}(x, targets)
methods::setMethod(
  "add_persistence_targets",
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
    add_persistence_targets(x, x$data$features[[targets]])
})
