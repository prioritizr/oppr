 #' @include internal.R OptimizationProblem-proto.R
NULL

#' Optimization problem methods
#'
#' These functions are used to access data from an
#' \code{\link{OptimizationProblem-class}} object.
#'
#' @param x \code{\link{OptimizationProblem-class}} object.
#'
#' @details The functions return the following data:
#'
#' \describe{
#'
#' \item{nrow}{\code{integer} number of rows (constraints).}
#'
#' \item{ncol}{\code{integer} number of columns (decision variables).}
#'
#' \item{ncell}{\code{integer} number of cells.}
#'
#' \item{modelsense}{\code{character} describing if the problem is to be
#'   maximized (\code{"max"}) or minimized (\code{"min"}).}
#'
#' \item{vtype}{\code{character} describing the type of each decision variable:
#'   binary (\code{"B"}), semi-continuous (\code{"S"}), or continuous
#'   (\code{"C"})}
#'
#' \item{obj}{\code{numeric} vector defining the linear components of the
#'   objective function.}
#'
#' \item{pwlobj}{\code{list} object defining the piece-wise linear components
#'   of the objective function.}
#'
#' \item{A}{\code{\link[Matrix]{dgCMatrix-class}} matrix object defining the
#'   problem matrix.}
#'
#' \item{rhs}{\code{numeric} vector with right-hand-side linear constraints}
#'
#' \item{sense}{\code{character} vector with the senses of the linear
#'   constraints (\code{"<="}, \code{">="}, \code{"="}).}
#'
#' \item{lb}{\code{numeric} lower bound for each decision variable. Missing data
#'   values (\code{NA}) indicate no lower bound for a given variable.}
#'
#' \item{ub}{\code{numeric} upper bounds for each decision variable. Missing
#'   data values (\code{NA}) indicate no upper bound for a given variable.}
#'
#' \item{number_of_projects}{\code{integer} number of projects in the problem.}
#'
#' \item{number_of_actions}{\code{integer} number of actions in the problem.}
#'
#' \item{number_of_features}{\code{integer} number of features in the problem.}
#'
#' \item{number_of_branches}{\code{integer} number of phylogenetic branches in
#'   the problem.}
#'
#' }
#'
#' @return \code{list}, \code{\link[Matrix]{dgCMatrix-class}}, \code{numeric}
#'   vector, \code{numeric} vector, or scalar \code{integer} depending on the
#'   method used.
#'
#' @name OptimizationProblem-methods
#'
#' @aliases nrow ncol ncell modelsense vtype obj pwlobj A rhs sense lb ub col_ids row_ids get_data number_of_branches ncell,OptimizationProblem-method A,OptimizationProblem-method col_ids,OptimizationProblem-method lb,OptimizationProblem-method modelsense,OptimizationProblem-method ncol,OptimizationProblem-method nrow,OptimizationProblem-method obj,OptimizationProblem-method pwlobj,OptimizationProblem-method rhs,OptimizationProblem-method row_ids,OptimizationProblem-method sense,OptimizationProblem-method ub,OptimizationProblem-method vtype,OptimizationProblem-method number_of_branches,OptimizationProblem-method get_data,OptimizationProblem-method
NULL

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage nrow(x)
if (!isS4(nrow)) methods::setGeneric("nrow",
                                     function(x) standardGeneric("nrow"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{nrow}{OptimizationProblem}(x)
methods::setMethod("nrow", "OptimizationProblem", function(x) x$nrow())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage ncol(x)
if (!isS4(ncol)) methods::setGeneric("ncol",
                                     function(x) standardGeneric("ncol"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{ncol}{OptimizationProblem}(x)
methods::setMethod("ncol", "OptimizationProblem", function(x) x$ncol())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage ncell(x)
if (!exists("ncell") || !isS4(ncell))
  methods::setGeneric("ncell", function(x) standardGeneric("ncell"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{ncell}{OptimizationProblem}(x)
methods::setMethod("ncell", "OptimizationProblem", function(x) x$ncell())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod modelsense
#'
#' @usage modelsense(x)
methods::setGeneric("modelsense", function(x) standardGeneric("modelsense"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{modelsense}{OptimizationProblem}(x)
methods::setMethod("modelsense", "OptimizationProblem",
  function(x) x$modelsense())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod vtype
#'
#' @usage vtype(x)
methods::setGeneric("vtype", function(x) standardGeneric("vtype"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{vtype}{OptimizationProblem}(x)
methods::setMethod("vtype", "OptimizationProblem", function(x) x$vtype())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod obj
#'
#' @usage obj(x)
methods::setGeneric("obj", function(x) standardGeneric("obj"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{obj}{OptimizationProblem}(x)
methods::setMethod("obj", "OptimizationProblem", function(x) x$obj())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod pwlobj
#'
#' @usage pwlobj(x)
methods::setGeneric("pwlobj", function(x) standardGeneric("pwlobj"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{pwlobj}{OptimizationProblem}(x)
methods::setMethod("pwlobj", "OptimizationProblem", function(x) x$pwlobj())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod A
#'
#' @usage A(x)
methods::setGeneric("A", function(x) standardGeneric("A"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{A}{OptimizationProblem}(x)
methods::setMethod("A", "OptimizationProblem", function(x) x$A())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod rhs
#'
#' @usage rhs(x)
methods::setGeneric("rhs", function(x) standardGeneric("rhs"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{rhs}{OptimizationProblem}(x)
methods::setMethod("rhs", "OptimizationProblem", function(x) x$rhs())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod sense
#'
#' @usage sense(x)
methods::setGeneric("sense", function(x) standardGeneric("sense"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{sense}{OptimizationProblem}(x)
methods::setMethod("sense", "OptimizationProblem", function(x) x$sense())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod lb
#'
#' @usage lb(x)
methods::setGeneric("lb", function(x) standardGeneric("lb"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{lb}{OptimizationProblem}(x)
methods::setMethod("lb", "OptimizationProblem", function(x) x$lb())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod ub
#'
#' @usage ub(x)
#'
methods::setGeneric("ub", function(x) standardGeneric("ub"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{ub}{OptimizationProblem}(x)
methods::setMethod("ub", "OptimizationProblem", function(x) x$ub())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod col_ids
#'
#' @usage col_ids(x)
methods::setGeneric("col_ids", function(x) standardGeneric("col_ids"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{col_ids}{OptimizationProblem}(x)
methods::setMethod("col_ids", "OptimizationProblem",
  function(x) x$col_ids())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod row_ids
#'
#' @usage row_ids(x)
methods::setGeneric("row_ids", function(x) standardGeneric("row_ids"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{row_ids}{OptimizationProblem}(x)
methods::setMethod("row_ids", "OptimizationProblem", function(x) x$row_ids())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod number_of_branches
#'
#' @usage number_of_branches(x)
#'
methods::setGeneric("number_of_branches", function(x)
  standardGeneric("number_of_branches"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{number_of_branches}{OptimizationProblem}(x)
methods::setMethod("number_of_branches", "OptimizationProblem", function(x)
  x$number_of_branches())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod get_data
#'
#' @usage get_data(x)
methods::setGeneric("get_data", function(x) standardGeneric("get_data"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{get_data}{OptimizationProblem}(x)
methods::setMethod("get_data", "OptimizationProblem", function(x) x$get_data())

#' Convert \code{OptimizationProblem} to list
#'
#' @param x \code{\link{OptimizationProblem-class}} object.
#'
#' @param ... not used.
#'
#' @return \code{\link{list}} object.
#'
#' @method as.list OptimizationProblem
#'
#' @rdname as.list
#'
#' @export
as.list.OptimizationProblem <- function(x, ...) {
  rcpp_optimization_problem_as_list(x$ptr)
}
