 #' @include internal.R OptimizationProblem-proto.R
NULL

#' Predefined optimization problem
#'
#' Create a new \code{\link{OptimizationProblem-class}} object.
#'
#' @param x \code{list} object containing data to construct the problem.
#'
#' @param data \code{list} object containing additional data (optional).
#'
#' @details The argument to \code{x} must be a list that contains the following
#'   elements:
#'
#'   \describe{
#'
#'   \item{modelsense}{\code{character} model sense.}
#'
#'   \item{number_of_projects}{\code{integer} number of projects in problem.}
#'
#'   \item{number_of_actions}{\code{integer} number of actions in problem.}
#'
#'   \item{number_of_features}{\code{integer} number of features in problem.}
#'
#'   \item{number_of_branches}{\code{integer} number of branches in problem.}
#'
#'   \item{A_i}{\code{integer} row indices for problem matrix.}
#'
#'   \item{A_j}{\code{integer} column indices for problem matrix.}
#'
#'   \item{A_x}{\code{numeric} values for problem matrix.}
#'
#'   \item{obj}{\code{numeric} vector defining the linear components of the
#'     objective function.}
#'
#'   \item{pwlobj}{\code{list} object defining the piece-wise linear components
#'      of the objective function.}
#'
#'   \item{lb}{\code{numeric} lower bound for decision values.}
#'
#'   \item{ub}{\code{numeric} upper bound for decision values.}
#'
#'   \item{rhs}{\code{numeric} right-hand side values.}
#'
#'   \item{sense}{\code{numeric} constraint senses.}
#'
#'   \item{vtype}{\code{character} variable types. These are used to specify
#'     that the decision variables are binary (\code{"B"}) or continuous
#'     (\code{"C"}).}
#'
#'   \item{row_ids}{\code{character} identifiers for the rows in the problem
#'     matrix.}
#'
#'   \item{col_ids}{\code{character} identifiers for the columns in the problem
#'     matrix.}
#'
#'   }
#'
#' @examples
#' # create list with problem data
#' l <- list(modelsense = "min", number_of_projects = 2,
#'           number_of_actions = 3, number_of_features = 1,
#'           number_of_branches = 5,
#'           A_i = c(0L, 1L, 0L, 1L, 0L, 1L), A_j = c(0L, 0L, 1L, 1L, 2L, 2L),
#'           A_x = c(2, 10, 1, 10, 1, 10), obj = c(1, 2, 2), lb = c(0, 1, 0),
#'           pwlobj = list(1), ub = c(0, 1, 1), rhs = c(2, 10),
#'           sense = c(">=", ">="), vtype = c("B", "B", "B"),
#'           row_ids = c("spp_target", "spp_target"),
#'           col_ids = c("pu", "pu", "pu"))
#'
#' # create list with additional data
#' d <- list(i = 4)
#'
#' # create OptimizationProblem object
#' x <- predefined_optimization_problem(l, d)
#'
#' # print new object
#' print(x)
#'
#' @noRd
predefined_optimization_problem <- function(x, data = list()) {
  assertthat::assert_that(inherits(x, "list"),
  assertthat::is.string(x$modelsense),
  identical(x$modelsense, "min") || identical(x$modelsense, "max"),
  assertthat::is.count(x$number_of_features), is.finite(x$number_of_features),
  assertthat::is.count(x$number_of_branches), is.finite(x$number_of_branches),
  assertthat::is.count(x$number_of_projects),
  is.finite(x$number_of_projects),
  assertthat::is.count(x$number_of_actions), is.finite(x$number_of_actions),
  is.numeric(x$obj), all(is.finite(x$obj)),
  is.numeric(x$lb), all(is.finite(x$lb)), length(x$obj) == length(x$lb),
  is.numeric(x$ub), all(is.finite(x$ub)), length(x$obj) == length(x$ub),
  is.list(x$pwlobj),
  all(x$lb <= x$ub),
  is.character(x$vtype), all(!is.na(x$vtype)),
  all(x$vtype %in% c("B", "S", "C")),
  is.numeric(x$rhs), all(is.finite(x$rhs)),
  length(x$rhs) >= length(x$number_of_features),
  is.character(x$sense), all(!is.na(x$sense)), length(x$sense) == length(x$rhs),
  all(x$sense %in% c("<=", "=", ">=")),
  is.character(x$row_ids), all(!is.na(x$row_ids)),
  length(x$row_ids) == length(x$rhs),
  is.character(x$col_ids), all(!is.na(x$col_ids)),
  length(x$col_ids) == length(x$obj),
  is.integer(x$A_i), all(is.finite(x$A_i)), min(x$A_i) == 0,
  max(x$A_i) == (length(x$rhs) - 1),
  is.integer(x$A_j), all(is.finite(x$A_j)), min(x$A_j) >= 0,
  max(x$A_j) <= (length(x$obj) - 1), length(x$A_i) == length(x$A_j),
  is.numeric(x$A_x), all(is.finite(x$A_x)), length(x$A_i) == length(x$A_x))
  pproto(NULL, OptimizationProblem,
         ptr = rcpp_predefined_optimization_problem(x),
         data = data)
}
