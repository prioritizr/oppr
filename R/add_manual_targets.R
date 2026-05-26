#' @include internal.R ProjectProblem-class.R tbl_df.R
NULL

#' Add manual targets
#'
#' Add targets to a project prioritization problem by manually
#' specifying detailed information for each target threshold.
#' Although this function is useful because it can be used to customize all
#' aspects of a target, it requires considerable more information
#' that other functions for adding targets (e.g., [add_absolute_targets()]
#' and [add_relative_targets()]).
#'
#' @param x [problem() object.
#'
#' @param targets `data.frame` or [tibble::tibble()] object. See
#' the Details section for more information.
#'
#' @details
#' Targets are used to specify a threshold minimum desirable
#' expected outcome for each feature. These should ideally be set
#' according to stakeholder requirements and expert knowledge.
#' Please note that attempting to solve problems with objectives that require
#' targets without specifying targets will throw an error.
#'
#' The argument to `targets` should contain the following columns:
#'
#' \describe{
#'
#' \item{`"feature"`}{
#' `character` values with names of features in `x`.
#' }
#'
#' \item{`"type"`}{
#' `character` values describing the type of target.
#' Acceptable values include `"absolute"` and `"relative"`.
#' }
#'
#' \item{`"sense"`}{
#' `character` values indicating the constraint sense for the target.
#' The only acceptable value currently supported is: `">="`.
#' This field (column) is optional and if it is missing then target senses will
#' default to `">="` values.
#' }
#'
#' \item{`"target"`}{`numeric` target threshold.}
#'
#' }
#'
#' @return A [problem()] object with the targets added to it.
#'
#' @family targets
#'
#' @seealso
#' See [targets] for an overview for functions for adding targets.
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # create data frame with targets
#' targets <- data.frame(
#'   feature = sim_features$name,
#'   type = "absolute",
#'   target = 0.1
#' )
#'
#' # print targets
#' print(targets)
#'
#' # build problem with minimum set objective and targets that require each
#' # feature to have a 30% chance of persisting into the future
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_manual_targets(targets) %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
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
  function(x, targets) standardGeneric("add_manual_targets")
)

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @usage \S4method{add_manual_targets}{ProjectProblem,data.frame}(x, targets)
methods::setMethod(
  "add_manual_targets",
  methods::signature("ProjectProblem", "data.frame"),
  function(x, targets) {
    add_manual_targets(x, tibble::as_tibble(targets))
  }
)

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @usage \S4method{add_manual_targets}{ProjectProblem,tbl_df}(x, targets)
methods::setMethod(
  "add_manual_targets",
  methods::signature("ProjectProblem", "tbl_df"),
  function(x, targets) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      inherits(targets, "tbl_df"),
      nrow(targets) > 0,
      ncol(targets) > 0,
      assertthat::has_name(targets, "feature"),
      assertthat::has_name(targets, "target"),
      assertthat::has_name(targets, "type"),
      is.character(targets$feature) || is.factor(targets$feature),
      all(as.character(targets$feature) %in% feature_names(x)),
      is.numeric(targets$target), all(is.finite(targets$target)),
      is.character(targets$type) || is.factor(targets$type),
      all(targets$type %in% c("absolute", "relative")),
      min(targets$target) >= 0
    )
    assertthat::assert_that(
      all(targets$target[targets$type == "relative"] <= 1),
      msg = "target values for relative targets must range between 0 and 1."
    )
    if (assertthat::has_name(targets, "sense")) {
      assertthat::assert_that(
        is.character(targets$sense) || is.factor(targets$sense),
        all(as.character(targets$sense) %in% c(">="))
      )
    }

    # add targets to problem
    x$add_targets(
      R6::R6Class(
        "ManualTargets",
        inherit = Target,
        public = list(
          name = "Targets",
          data = list(
            targets = targets,
            feature_names = x$feature_names(),
            max_eof = apply(x$eof_matrix(), 2, max)
          ),
          repr = function() {
            targets <- self$get_data("targets")
            if (all(as.character(targets$type) == "relative")) {
              out <- "relative"
            } else if (all(as.character(targets$type) == "absolute")) {
              out <- "absolute"
            } else {
              out <- "mixed"
            }
            paste0(out, " targets")
          },
          output = function() {
            # get data
            targets <- self$get_data("targets")
            max_eof <- self$get_data("max_eof")
            feature_names <- self$get_data("feature_names")
            # add sense column if missing
            if (!assertthat::has_name(targets, "sense")) {
              targets$sense <- ">="
            }
            targets$sense <- as.character(targets$sense)
            # add targets for missing features
            # these targets are -1 so they should always be met
            missing_features <- setdiff(
              feature_names, as.character(targets$feature)
            )
            if (length(missing_features) > 0) {
              targets <- rbind(
                targets,
                tibble::tibble(
                  feature = missing_features,
                  type = "absolute",
                  sense = ">=",
                  target = -1
                )
              )
              targets <- tibble::as_tibble(targets)
            }
            # convert feature names to indices
            targets$feature <- match(targets$feature, feature_names)
            # add compute relative targets as absolute targets
            targets$value <- as.numeric(targets$target)
            relative_rows <- which(targets$type == "relative")
            for (i in seq_along(relative_rows)) {
              feature_id <- targets$feature[[relative_rows[[i]]]]
              targets$value[relative_rows[i]] <-
                max_eof[feature_id] *
                  targets$target[relative_rows[i]]
            }
            # return tibble
            targets[, c("feature", "sense", "value")]
          }
        )
      )$new()
    )
  }
)
