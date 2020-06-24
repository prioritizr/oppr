#' @include internal.R pproto.R Parameters-proto.R
NULL

#' @export
if (!methods::isClass("OptimizationProblem")) methods::setOldClass("OptimizationProblem")
NULL

#' Optimization problem class
#'
#' The `OptimizationProblem` class is used to represent an optimization
#' problem. Data are stored in memory and accessed using an external pointer.
#' **Only experts should interact with this class directly.**
#'
#' @section Fields:
#' \describe{
#'
#' \item{$ptr}{`externalptr` object.}
#'
#' \item{$data}{`list` object.}
#'
#' }
#'
#' @section Usage:
#' `x$print()`
#'
#' `x$show()`
#'
#' `x$repr()`
#'
#' `x$ncol()`
#'
#' `x$nrow()`
#'
#' `x$ncell()`
#'
#' `x$modelsense()`
#'
#' `x$vtype()`
#'
#' `x$obj()`
#'
#' `x$pwlobj()`
#'
#' `x$A()`
#'
#' `x$rhs()`
#'
#' `x$sense()`
#'
#' `x$lb()`
#'
#' `x$ub()`
#'
#' `x$number_of_projects()`
#'
#' `x$number_of_actions()`
#'
#' `x$number_of_features()`
#'
#' `x$number_of_branches()`
#'
#' `x$row_ids()`
#'
#' `x$col_ids()`
#'
#' `x$get_data()`
#'
#' @section Arguments:
#' \describe{
#' \item{ptr}{`externalptr` object.}
#' }
#'
#' @section Details:
#' \describe{
#'
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{`character` representation of object.}
#'
#' \item{ncol}{`integer` number of columns (variables) in model matrix.}
#'
#' \item{nrow}{`integer` number of rows (constraints) in model matrix.}
#'
#' \item{ncell}{`integer` number of cells in model matrix.}
#'
#' \item{modelsense}{`character` model sense.}
#'
#' \item{vtype}{`character` vector of variable types.}
#'
#' \item{obj}{`numeric` vector containing the linear components of the
#'   objective function.}
#'
#' \item{pwlobj}{`list` object containing the piece-wise linear components
#'   of the objective function.}
#'
#' \item{A}{[Matrix::dgCMatrix-class] model matrix }
#'
#' \item{rhs}{`numeric` vector of right-hand-side constraints.}
#'
#' \item{sense}{`character` vector of constraint senses.}
#'
#' \item{lb}{`numeric` vector of lower bounds for each decision variable.}
#'
#' \item{ub}{`numeric` vector of upper bounds for each decision variable.}
#'
#' \item{number_of_projects}{`integer` number of projects in the problem.}
#'
#' \item{number_of_actions}{`integer` number of actions in the problem.}
#'
#' \item{number_of_features}{`integer` number of features in the problem.}
#'
#' \item{number_of_branches}{`integer` number of phylogenetic branches in
#'   the problem.}
#'
#' \item{col_ids}{`character` names describing each decision variable
#'   (column) in the model matrix.}
#'
#' \item{row_ids}{`character` names describing each constraint (row) in
#'   in the model matrix.}
#'
#' \item{get_data}{`list` containing additional data.}
#'
#' }
#'
#' @name OptimizationProblem-class
#'
#' @aliases OptimizationProblem
NULL

#' @export
OptimizationProblem <- pproto(
  "OptimizationProblem",
  ptr = NULL,
  data = list(),
  print = function(self) {
    if (self$ncol() > 0) {
    cv <- table(self$vtype())
    cv <- paste(paste(unname(cv), paste0("(", names(cv), ")")),
          collapse = ", ")
    message("optimization problem",
      "\n  objective:    ", ifelse(length(self$pwlobj()) == 0, "linear",
                           "piece-wise linear"),
      "\n  model sense: ", self$modelsense(),
      "\n  dimensions:  ", self$nrow(), ", ", self$ncol(), ", ", self$ncell(),
                          " (nrow, ncol, ncell)",
      "\n  variables:   ", cv)
    } else {
      message("optimization problem (empty)")
    }
  },
  show = function(self) {
    self$print()
  },
  ncol = function(self) {
    rcpp_get_optimization_problem_ncol(self$ptr)
  },
  nrow = function(self) {
    rcpp_get_optimization_problem_nrow(self$ptr)
  },
  ncell = function(self) {
    rcpp_get_optimization_problem_ncell(self$ptr)
  },
  modelsense = function(self) {
    rcpp_get_optimization_problem_modelsense(self$ptr)
  },
  vtype = function(self) {
    rcpp_get_optimization_problem_vtype(self$ptr)
  },
  obj = function(self) {
    rcpp_get_optimization_problem_obj(self$ptr)
  },
  pwlobj = function(self) {
    rcpp_get_optimization_problem_pwlobj(self$ptr)
  },
  A = function(self) {
    x <- rcpp_get_optimization_problem_A(self$ptr)
    Matrix::sparseMatrix(i = x$i, j = x$j, x = x$x, index1 = FALSE,
                         giveCsparse = TRUE,
                         dims = c(nrow(self), ncol(self)))
  },
  rhs = function(self) {
    rcpp_get_optimization_problem_rhs(self$ptr)
  },
  sense = function(self) {
    rcpp_get_optimization_problem_sense(self$ptr)
  },
  lb = function(self) {
    rcpp_get_optimization_problem_lb(self$ptr)
  },
  ub = function(self) {
    rcpp_get_optimization_problem_ub(self$ptr)
  },
  number_of_features = function(self) {
    rcpp_get_optimization_problem_number_of_features(self$ptr)
  },
  number_of_branches = function(self) {
    rcpp_get_optimization_problem_number_of_branches(self$ptr)
  },
  number_of_actions = function(self) {
    rcpp_get_optimization_problem_number_of_actions(self$ptr)
  },
  number_of_projects = function(self) {
    rcpp_get_optimization_problem_number_of_projects(self$ptr)
  },
  col_ids = function(self) {
    rcpp_get_optimization_problem_col_ids(self$ptr)
  },
  row_ids = function(self) {
    rcpp_get_optimization_problem_row_ids(self$ptr)
  },
  get_data = function(self) {
    self$data
  }
)
