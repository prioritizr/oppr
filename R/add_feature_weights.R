#' @include internal.R ProjectProblem-class.R
NULL

#' Add feature weights
#'
#' Add weights to the features in a project prioritization problem.
#'
#' @param x [problem()] object.
#'
#' @param weights Object that specifies the weights for each feature. See the
#' Details section for more information.
#'
#' @details
#' Weights are used to specify the relative importance of particular
#' features. For budget constrained problems
#'  (e.g., [add_max_wtd_sum_objective()]), these weights
#' could be used to specify which features are more important than other
#' features according to evolutionary or cultural metrics. Specifically,
#' features with a higher weight value are considered more important. It is
#' generally best to ensure that weight values range between 0.001 and
#' 1,000 to reduce the time required to solve problems using exact
#' algorithm solvers. As a consequence, you might have to rescale the feature
#' weights if the largest or smallest values occur outside this range
#' (excluding zeros). If you want to ensure that certain features conserved
#' in the solutions, it is strongly recommended to lock in the actions for
#' these features instead of setting really high weights for these features.
#' Please note that a warning will be thrown if you attempt to solve
#' problems with weights when an objective has been specified that does
#' not use weights. Currently, all objectives -- except for the minimum
#' set objective (i.e., [add_min_set_objective()]) -- can use weights.
#'
#' The weights for a problem can be specified in several different ways:
#'
#' \describe{
#'
#' \item{`numeric` vector}{
#' Values specify a weight for each feature.
#' These `numeric` values should correspond to each row in the argument to
#' `features` for [problem()].
#' }
#'
#' \item{`character` value}{
#' The value specifies the name of a column in the feature data
#' (i.e., argument to `features` in [problem()]). The column must
#' have `numeric` values, and these used to weight the features.
#' }
#'
#' }
#'
#' @return A [problem()] with the weights added to it.
#'
#' @family weights
#'
#' @seealso
#' See [weights] for an overview of functions for adding weights.
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # print feature data
#' print(sim_features)
#'
#' # build problem with maximum weighted sum objective, $300 budget,
#' # and no weights
#' p1 <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p1)
#'
#' # build another problem, and specify feature weights using the values in the
#' # "weight" column of the sim_features table by specifying the column
#' # name "weight"
#' p2 <- p1 %>% add_feature_weights("weight")
#'
#' # print problem
#' print(p2)
#'
#' # build another problem, and specify feature weights using the
#' # values in the "weight column of the sim_features table, but
#' # actually input the values rather than specifying the column name
#' # "weights" column of the sim_features table
#' p3 <- p1 %>% add_feature_weights(sim_features$weight)
#'
#' # print problem
#' print(p3)
#'
#' # solve the problems
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
#' @aliases add_feature_weights,ProjectProblem,numeric-method add_feature_weights,ProjectProblem,character-method
#'
#' @name add_feature_weights
#'
NULL

#' @name add_feature_weights
#' @rdname add_feature_weights
#' @exportMethod add_feature_weights
#' @export
methods::setGeneric(
  "add_feature_weights",
  signature = methods::signature("x", "weights"),
  function(x, weights) standardGeneric("add_feature_weights")
)

#' @name add_feature_weights
#' @rdname add_feature_weights
#' @usage \S4method{add_feature_weights}{ProjectProblem,numeric}(x, weights)
methods::setMethod(
  "add_feature_weights",
  methods::signature("ProjectProblem", "numeric"),
  function(x, weights) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      is.numeric(weights),
      length(weights) == number_of_features(x),
      assertthat::noNA(weights)
    )
    # add weights
    x$add_weights(
      R6::R6Class(
        "FeatureWeights",
        inherit = Weight,
        public = list(
          name = "feature weights",
          data = list(
            weights = stats::setNames(weights, x$feature_names())
          ),
          output = function() {
            self$get_data("weights")
          },
          apply = function(x, y) {
            assertthat::assert_that(
              inherits(x, "OptimizationProblem"),
              inherits(y, "ProjectProblem")
            )
            invisible(rcpp_apply_feature_weights(
              x$ptr, self$get_data("weights")[y$feature_phylogeny()$tip.label],
              y$objective$replace_feature_weights()
            ))
          }
        )
      )$new()
    )
  }
)

#' @name add_feature_weights
#' @rdname add_feature_weights
#' @usage \S4method{add_feature_weights}{ProjectProblem,character}(x, weights)
methods::setMethod(
  "add_feature_weights",
  methods::signature("ProjectProblem", "character"),
  function(x, weights) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ProjectProblem"),
      assertthat::is.string(weights),
      assertthat::noNA(weights),
      assertthat::has_name(x$data$features, weights),
      is.numeric(x$data$features[[weights]]),
      assertthat::noNA(x$data$features[[weights]])
    )
    # add weights to problem
    add_feature_weights(x, x$data$features[[weights]])
  }
)
