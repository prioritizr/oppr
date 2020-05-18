#' @include internal.R pproto.R ProjectProblem-proto.R MiscParameter-proto.R tbl_df.R
NULL

#' Add manual targets
#'
#' Set targets for a project prioritization \code{\link{problem}} by manually
#' specifying all the required information for each target. This function
#' is useful because it can be used to customize all aspects of a target. For
#' most cases, targets can be specified using the
#' \code{\link{add_absolute_targets}} and \code{\link{add_relative_targets}}
#' functions. However, this function can be used to mix absolute and
#' relative targets for different features.
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param targets \code{data.frame} or \code{\link[tibble]{tibble}} object. See
#'   the Details section for more information.
#'
#' @details Targets are used to specify the minimum probability of persistence
#'   for each feature in solutions. For minimum set objectives
#'   (i.e. \code{\link{add_min_set_objective}}, these targets
#'   specify the minimum probability of persistence required for each species
#'   in the solution. And for budget constrained objectives that use targets
#'   (i.e. \code{\link{add_max_targets_met_objective}}), these targets
#'   specify the minimum threshold probability of persistence that needs to be
#'   achieved to count the benefits for conserving these species.
#'   Please note that attempting to solve problems with objectives that require
#'   targets without specifying targets will throw an error.
#'
#'   The \code{targets} argument should contain the following columns:
#'
#'   \describe{
#'
#'   \item{\code{"feature"}}{\code{character} name of features in argument
#'     to \code{x}.}
#'
#'   \item{\code{"type"}}{\code{character} describing the type of target.
#'     Acceptable values include \code{"absolute"} and \code{"relative"}.
#'     These values correspond to \code{\link{add_absolute_targets}},
#'     and \code{\link{add_relative_targets}} respectively.}
#'
#'   \item{\code{"sense"}}{\code{character} sense of the target. The
#'     only acceptable value currently supported is: \code{">="}. This field
#'     (column) is optional and if it is missing then target senses will
#'     default to \code{">="} values.}
#'
#'   \item{\code{"target"}}{\code{numeric} target threshold.}
#'
#'   }
#'
#' @return \code{\link{ProjectProblem-class}} object with the targets added
#'   to it.
#'
#' @inherit add_absolute_targets seealso return
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#'
#' # create data frame with targets
#' targets <- data.frame(feature = sim_features$name,
#'                       type = "absolute",
#'                       target = 0.1)
#'
#' # print targets
#' print(targets)
#'
#' # build problem with minimum set objective and targets that require each
#' # feature to have a 30% chance of persisting into the future
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'       add_min_set_objective() %>%
#'       add_manual_targets(targets) %>%
#'       add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
#' \donttest{
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#' }
#' @aliases add_manual_targets-method add_manual_targets,ProjectProblem,data.frame-method add_manual_targets,ProjectProblem,tbl_df-method
#'
#' @name add_manual_targets
#'
#' @docType methods
NULL

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @exportMethod add_manual_targets
#' @export
methods::setGeneric(
  "add_manual_targets",
  signature = methods::signature("x", "targets"),
  function(x, targets) standardGeneric("add_manual_targets"))

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @usage \S4method{add_manual_targets}{ProjectProblem,data.frame}(x, targets)
methods::setMethod(
  "add_manual_targets",
  methods::signature("ProjectProblem", "data.frame"),
  function(x, targets) {
    add_manual_targets(x, tibble::as_tibble(targets))
})

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @usage \S4method{add_manual_targets}{ProjectProblem,tbl_df}(x, targets)
methods::setMethod(
  "add_manual_targets",
  methods::signature("ProjectProblem", "tbl_df"),
  function(x, targets) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "ProjectProblem"))
    validate_targets <- function(targets) {
      assertthat::assert_that(
        inherits(targets, "tbl_df"),
        nrow(targets) > 0, ncol(targets) > 0,
        assertthat::has_name(targets, "feature"),
        assertthat::has_name(targets, "target"),
        assertthat::has_name(targets, "type"),
        all(names(targets) %in% c("feature", "type", "sense", "target")),
        is.character(targets$feature) || is.factor(targets$feature),
        all(as.character(targets$feature) %in% feature_names(x)),
        is.numeric(targets$target), all(is.finite(targets$target)),
        is.character(targets$type) || is.factor(targets$type),
        all(targets$type %in% c("absolute", "relative")),
        min(targets$target) >= 0,
        max(targets$target) <= 1)
      if (assertthat::has_name(targets, "sense"))
       assertthat::assert_that(
         is.character(targets$sense) || is.factor(targets$sense),
         all(as.character(targets$sense) %in% c(">=")))
      return(TRUE)
    }
    validate_targets(targets)
    # define function to validate changes to the targets object
    vfun <- function(x) !inherits(try(validate_targets(x), silent = TRUE),
                                  "try-error")
    # define function to render targets object
    rfun <- function(x)
      utils::getFromNamespace("rHandsontableOutput", "rhandsontable")(x)
    # add targets to problem
    x$add_targets(pproto(
    "ManualTargets",
    Target,
    name = "Targets",
    data = list(max_persistences = apply(x$epf_matrix(), 2, max),
                feature_names = x$feature_names()),
    parameters = parameters(misc_parameter("Targets", targets, vfun, rfun)),
    repr = function(self) {
      targets <- self$parameters$get("Targets")
      if (all(as.character(targets$type) == "relative")) {
        out <- "Relative"
      } else if (all(as.character(targets$type) == "absolute")) {
        out <- "Absolute"
      } else {
        out <- "Mixed"
      }
      out <- paste0(out, " targets [targets (min: ", min(targets$target),
                    ", max: ", max(targets$target), ")]")
      return(out)
     },
     output = function(self) {
       # get data
       targets <- self$parameters$get("Targets")
       max_persistences <- self$data$max_persistences
       feature_names <- self$data$feature_names
       # add sense column if missing
       if (!assertthat::has_name(targets, "sense"))
         targets$sense <- ">="
       targets$sense <- as.character(targets$sense)
       # add targets for missing features
       # these targets are -1 so they should always be met
       missing_features <- setdiff(feature_names,
                                   as.character(targets$feature))
       if (length(missing_features) > 0) {
         targets <- rbind(targets, tibble::tibble(feature = missing_features,
                                                  type = "absolute",
                                                  sense = ">=", target = -1))
         targets <- tibble::as_tibble(targets)
       }
       # convert feature names to indices
       targets$feature <- match(targets$feature, feature_names)
       # add compute relative targets as absolute targets
       targets$value <- as.numeric(targets$target)
       relative_rows <- which(targets$type == "relative")
       for (i in seq_along(relative_rows)) {
          feature_id <- targets$feature[[relative_rows[[i]]]]
          targets$value[relative_rows[i]] <- max_persistences[feature_id] *
                                             targets$target[relative_rows[i]]
       }
       # return tibble
       return(targets[, c("feature", "sense", "value")])
     }))
})
