#' @include internal.R pproto.R ProjectProblem-proto.R
NULL

#' Add weights
#'
#' Set feature weights for a project prioritization \code{\link{problem}}.
#'
#' @param x \code{\link{ProjectProblem-class}} object.
#'
#' @param weights Object that specifies the weights for each feature. See the
#'   Details section for more information.
#'
#' @details Weights are used to specify the relative importance for
#'   maintaining the persistence of different features. For budget constrained
#'   problems (e.g. \code{\link{add_max_richness_objective}}, these weights
#'   could be used to specify which features are more important than other
#'   features according to evolutionary or cultural metrics. Specifically,
#'   features with a higher weight value are considered more important. It is
#'   generally best to ensure that the feature weights range between 0.0001 and
#'   10,000 to reduce the time required to solve problems using integer
#'   programming. As a consequence, you might have to rescale the feature
#'   weights if the largest or smallest values occur outside this range
#'   (excluding zeros). If you want to ensure that certain features conserved
#'   in the solutions, it is strongly recommended to lock in the actions for
#'   these features instead of setting really high weights for these features.
#'   Please note that a warning will be thrown if you attempt to solve
#'   problems with weights when an objective has been specified that does
#'   not use weights. Currently, all objectives---except for the minimum
#'   set (i.e. \code{\link{add_min_set_objective}} and the
#'   the maximum expected phylogenetic diversity (i.e.
#'   \code{\link{add_max_phylo_objective}}) objectives---can use weights.
#'
#'   The weights for a problem can be specified in several different ways:
#'
#'   \describe{
#'
#'   \item{\code{numeric}}{\code{vector} of weight values for each feature.}
#'
#'   \item{\code{character}}{specifying the name of column in the
#'     feature data (i.e. the argument to \code{features} in the
#'     \code{\link{problem}} function) that contains the weights.}
#'
#'   }
#'
#' @seealso \code{\link{weights}}.
#'
#' @examples
#' #TODO
#'
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
  function(x, weights) standardGeneric("add_feature_weights"))

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
      assertthat::noNA(weights))
    # add weights
    x$add_penalty(pproto(
      "FeatureWeights",
      Penalty,
      name = "Feature weights",
      parameters = parameters(numeric_parameter_array("weights", weights,
                              x$feature_names(), lower_limit = 0)),
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
          inherits(y, "ProjectProblem"))
          weights <- c(as.matrix(self$parameters$get("weights")))
          assertthat::assert_that(length(weights) == y$number_of_features(),
              msg = paste0("the number of weights must correspond to ",
                           "the number of features in the problem"))
          names(weights) < y$feature_names()
        invisible(rcpp_apply_feature_weights(
          x$ptr, weights[x$feature_phylogeny()$tip.label]))
      }))
})

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
      assertthat::noNA(x$data$features[[weights]]))
    # add weights to problem
    add_feature_weights(x, x$data$features[[weights]])
})
