#' @include internal.R
NULL

#' Feature names
#'
#' Extract the names of the features in an object.
#'
#' @param x \code{\link{ProjectProblem-class}}.
#'
#' @return \code{character} feature names.
#'
#' @name feature_names
#'
#' @aliases feature_names,ProjectProblem-method
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with default solver
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions() %>%
#'      add_default_solver()
#'
#' # print problem
#' print(p)
#'
#' # print feature names
#' feature_names(p)
NULL

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @exportMethod feature_names
#'
#' @usage feature_names(x)
#'
methods::setGeneric("feature_names",
                    function(x) standardGeneric("feature_names"))

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{ProjectProblem}(x)
#'
methods::setMethod("feature_names", "ProjectProblem",
  function(x) x$feature_names())
