#' @include internal.R ProjectProblem-proto.R OptimizationProblem-proto.R compile.R
NULL

#' Solve
#'
#' Solve a conservation planning \code{\link{problem}}.
#'
#' @param a \code{\link{ProjectProblem-class}} or an
#'   \code{\link{OptimizationProblem-class}} object.
#'
#' @param b \code{\link{Solver-class}} object. Not used if \code{a} is an
#'   \code{\link{ProjectProblem-class}} object.
#'
#' @param ... arguments passed to \code{\link{compile}}.
#'
#' @return The object returned from this function depends on the argument to
#'   \code{a}. If the argument to \code{a} is an
#'   \code{\link{OptimizationProblem-class}} object, then the
#'   solution is returned as a \code{logical} \code{vector} indicating which
#'   actions are funded or not. On the other hand, if the argument
#'   to \code{a} is an \code{\link{ProjectProblem-class}} object,
#'   then a \code{\link[tibble]{tibble}} table object will be returned. In this
#'   table, each row row corresponds to a different solution and each column
#'   describes a different property or result associated with each solution:
#'
#'   \describe{
#'
#'   \item{\code{"solution"}}{}
#'
#'   \item{\code{"solution"}}{\code{integer} solution identifier.}
#'
#'   \item{\code{"status"}}{\code{character} describing each solution.
#'    For example, is the solution optimal, suboptimal, or was it returned
#'    because the solver ran out of time?}
#'
#'   \item{\code{"cost"}}{\code{numeric} total cost associated with each
#'     solution.}
#'
#'   \item{code{...}}{columns for each action which \code{logical}} values
#'     (i.e. \code{TRUE} or \code{FALSE}) that indicate if each action
#'     action was funded in each solution.
#'
#'   }
#'
#'
#' @seealso \code{\link{evaluate}}, \code{\link{problem}},
#'   \code{\link{solvers}}.
#'
#' @examples
#' #TODO
#'
#' @name solve
#'
#' @importFrom Matrix solve
#'
#' @exportMethod solve
#'
#' @aliases solve,OptimizationProblem,Solver-method solve,ProjectProblem,missing-method
#'
#' @export
NULL

#' @name solve
#'
#' @rdname solve
methods::setMethod(
  "solve",
  signature(a = "OptimizationProblem", b = "Solver"),
  function(a, b, ...) b$solve(a)
)

#' @name solve
#'
#' @rdname solve
methods::setMethod(
  "solve",
  signature(a = "ProjectProblem", b = "missing"),
  function(a, b, ...) {
    ## solve problem
    # assign solver
    if (inherits(a$solver, "Waiver"))
      a <- add_default_solver(a)
    # compile and solve optimisation problem
    opt <- compile.ProjectProblem(a, ...)
    sol <- a$solver$solve(opt)
    o1 <<- sol
    # check that solution is valid
    if (is.null(sol) || is.null(sol[[1]]$x)) {
      stop("project prioritization problem is infeasible")
    }
    ## format solutions
    # extract actions
    action_status <- lapply(sol,
      function(x) matrix(x[[1]][seq_len(a$number_of_actions())], nrow = 1))
    if (length(action_status) == 1) {
      action_status <- action_status[[1]]
    } else {
      action_status <- do.call(rbind, action_status)
    }
    # create solution data
    ## initialize and add solution column
    out <- tibble::tibble(solution = seq_len(nrow(action_status)))
    ## add status column
    out$status <- vapply(sol, `[[`, character(1), 3)
    ## add cost column
    out$cost <- rowSums(action_status * matrix(a$action_costs(), byrow = TRUE,
                                               nrow = nrow(action_status),
                                               ncol = ncol(action_status)))
    ## add solution columns
    s <- tibble::as.tibble(as.data.frame(action_status))
    names(s) <- a$action_names()
    out <- tibble::as.tibble(cbind(out, s))

    # return result
    out
  }
)
