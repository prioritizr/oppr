#' @include internal.R ProjectProblem-class.R MultiObjProjectProblem-class.R
NULL

#' Number of features
#'
#' Get the number of features in an object.
#'
#' @inheritParams number_of_actions
#'
#' @return An `integer` value.
#'
#' @name number_of_features
#'
#' @aliases number_of_features,ProjectProblem-method  number_of_features,MultiObjProjectProblem-method
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
#' # print number of features
#' number_of_features(p)
NULL

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @exportMethod number_of_features
#'
#' @usage number_of_features(x)
methods::setGeneric(
  "number_of_features",
  function(x) standardGeneric("number_of_features")
)

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @usage \S4method{number_of_features}{ProjectProblem}(x)
methods::setMethod(
  "number_of_features", "ProjectProblem",
  function(x) x$number_of_features()
)

#' @name number_of_features
#'
#' @rdname number_of_features
#'
#' @usage \S4method{number_of_features}{MultiObjProjectProblem}(x)
methods::setMethod(
  "number_of_features", "MultiObjProjectProblem",
  function(x) x$number_of_features()
)
