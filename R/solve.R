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
#' @return The type of object returned from this function depends on the
#'   argument to \code{a}. If the argument to \code{a} is an
#'   \code{\link{OptimizationProblem-class}} object, then the
#'   solution is returned as a \code{list} containing the prioritization and
#'   additional information (e.g. run time, solver status). On the other hand,
#'   if the argument
#'   to \code{a} is an \code{\link{ProjectProblem-class}} object,
#'   then a \code{\link[tibble]{tibble}} table object will be returned. In this
#'   table, each row row corresponds to a different solution and each column
#'   describes a different property or result associated with each solution:
#'
#'   \describe{
#'
#'   \item{\code{"solution"}}{\code{integer} solution identifier.}
#'
#'   \item{\code{"status"}}{\code{character} describing each solution.
#'    For example, is the solution optimal, suboptimal, or was it returned
#'    because the solver ran out of time?}
#'
#'   \item{\code{"obj"}}{\code{numeric} objective value for each solution.
#'     This is calculated using the objective function defined for the
#'     argument to \code{x}.}
#'
#'   \item{\code{"cost"}}{\code{numeric} total cost associated with each
#'     solution.}
#'
#'   \item{\code{x$action_names()}}{\code{numeric} column for each action
#'     indicating if they were funded in each solution or not.}
#'
#'   \item{\code{x$project_names()}}{\code{numeric} column for each
#'     project indicating if it was completely funded (with a value of 1)
#'     or not (with a value of 0).}
#'
#'   \item{\code{x$feature_names()}}{\code{numeric} column for each
#'     feature indicating the probability that it will persist into
#'     the future given each solution.}
#'
#'   }
#'
#' @seealso \code{\link{problem}}, \code{\link{solution_statistics}},
#'   \code{\link{solvers}}.
#'
#' @name solve
#'
#' @importFrom Matrix solve
#'
#' @exportMethod solve
#'
#' @aliases solve,OptimizationProblem,Solver-method solve,ProjectProblem,missing-method
#'
#' @examples
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # print project data
#' print(sim_projects)
#'
#' # print action data
#' print(sim_features)
#'
#' # print feature data
#' print(sim_actions)
#'
#' # build problem
#' p <- problem(sim_projects, sim_actions, sim_features,
#'              "name", "success", "name", "cost", "name") %>%
#'      add_max_richness_objective(budget = 400) %>%
#'      add_feature_weights("weight") %>%
#'      add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
#' \donttest{
#' # solve problem
#' s <- solve(p)
#'
#' # print output
#' print(s)
#'
#' # print the solver status
#' print(s$obj)
#'
#' # print the objective value
#' print(s$obj)
#'
#' # print the solution cost
#' print(s$cost)
#'
#' # print which actions are funded in the solution
#' s[, sim_actions$name, drop = FALSE]
#'
#' # print the expected probability of persistence for each feature
#' # if the solution were implemented
#' s[, sim_features$name, drop = FALSE]
#' }
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
    ### remove duplicate solutions if not using random solver
    if (!inherits(a$solver, "RandomSolver")) {
      not_dups <- !duplicated(apply(action_status, 1, paste, collapse = "_"))
      action_status <- action_status[not_dups, , drop = FALSE]
      sol <- sol[not_dups]
    }
    # create solution data
    ## initialize and add solution column
    out <- tibble::tibble(solution = seq_len(nrow(action_status)))
    ## add status column
    out$status <- vapply(sol, `[[`, character(1), 3)
    ## add solution columns
    s <- tibble::as_tibble(as.data.frame(action_status))
    names(s) <- a$action_names()
    out <- tibble::as_tibble(cbind(out, s))
    ### add remaining columns
    out <- tibble::as_tibble(cbind(out, solution_statistics(a, s)))
    ### reorder columns
    out <- out[, c("solution", "status", "obj", "cost", a$action_names(),
                   a$project_names(), a$feature_names())]
    # return result
    out
  }
)
