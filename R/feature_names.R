#' @include internal.R ProjectProblem-class.R MultiObjProjectProblem-class.R
NULL

#' Feature names
#'
#' Get the names of the features in an object.
#'
#' @inheritParams action_names
#'
#' @return A `character` vector or `list` of `character` vectors.
#'
#' @name feature_names
#'
#' @aliases feature_names,ProjectProblem-method feature_names,MultiObjProjectProblem-method
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
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
methods::setGeneric(
  "feature_names",
  function(x) standardGeneric("feature_names")
)

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{ProjectProblem}(x)
methods::setMethod(
  "feature_names", "ProjectProblem",
  function(x) x$feature_names()
)

#' @name feature_names
#'
#' @rdname feature_names
#'
#' @usage \S4method{feature_names}{MultiObjProjectProblem}(x)
methods::setMethod(
  "feature_names", "MultiObjProjectProblem",
  function(x) x$feature_names()
)
