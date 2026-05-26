#' @include internal.R ProjectProblem-class.R OptimizationProblem-class.R MultiObjProjectProblem-class.R compile.R
NULL

#' Solve
#'
#' Solve a conservation planning [problem()].
#'
#' @param a [problem()], [multi_problem()], or [OptimizationProblem-class]
#' object.
#'
#' @param b [Solver-class] object. Note that this parameter is only used
#' if `a` is an [OptimizationProblem-class] object.
#'
#' @param ... arguments passed to [compile()].
#'
#' @return
#' The type of object returned from this function depends on the
#' argument to `a`. If the argument to `a` is an
#' [OptimizationProblem-class] object, then the
#' solution is returned as a `list` containing the prioritization and
#' additional information (e.g., run time, solver status). On the other hand,
#' if the argument to `a` is a [problem()] or [multi_problem()] object,
#' then a [tibble::tibble()] object will be returned. In this
#' table, each row row corresponds to a different solution and each column
#' describes a different property or result associated with each solution.
#' In particular, it will have the following columns.
#'
#' \describe{
#'
#' \item{`"solution"`}{
#' This column contains `integer` identifiers for the solutions.
#' }
#'
#' \item{`"status"`}{
#' This column contains `character` values that describe the solver status.
#' For example, these values may indicate if the solver returned
#' an optimal or suboptimal solution.
#' }
#'
#' \item{`"obj"`}{
#' This column contains `numeric` values that contain the objective
#' value for each solution. This is calculated using the objective function
#' defined for the argument to `a`. Note that if `a` is a [multi_problem()]
#' object, then an objective column will be created for each problem in `a`.
#' }
#'
#' \item{`"cost"`}{
#' This column contains `numeric` values that describe the total cost
#' associated with each solution.
#' }
#'
#' \item{`x$action_names()`}{
#' These columns contain `logical` (`TRUE`/`FALSE`) values that indicate if each
#' for each action was selected for funding (or not) by each solution.
#' }
#'
#' \item{`x$project_names()`}{
#' These columns contain `logical` (`TRUE`/`FALSE`) values that indicate if each
#' for each project had all of its actions selected for funding (or not) by
#' each solution.
#' }
#'
#' \item{`x$feature_names()`}{`
#' These columns contain `numeric` values that describe the expected outcome
#' for each feature based on the actions selected for funding.
#' }
#'
#' }
#'
#' @seealso
#' The [solution_statistics()] function can be used to compute these
#' statistics for solutions. This may be useful to evaluate the performance of
#' solutions generated based on expert opinion, or solutions according
#' to objectives that are different from those used to generate them.
#
#' @name solve
#'
#' @importFrom Matrix solve
#'
#' @exportMethod solve
#'
#' @aliases solve,OptimizationProblem,Solver-method solve,ProjectProblem,missing-method solve,MultiObjProjectProblem,missing-method
#'
#' @examplesIf oppr::run_example()
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
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 400) %>%
#'   add_feature_weights("weight") %>%
#'   add_binary_decisions()
#'
#' # print problem
#' print(p)
#'
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
    if (inherits(a$solver, "Waiver")) {
      a <- add_default_solver(a) # nocov
    }
    # compile and solve optimisation problem
    opt <- compile.ProjectProblem(a, ...)
    if (!isTRUE(a$solver$has_pwlobj)) {
      opt$convert_pwlobj()
    }
    sol <- a$solver$solve(opt)
    # check that solution is valid
    if (is.null(sol) || is.null(sol[[1]]$x)) {
      stop("project prioritization problem is infeasible")
    }
    ## format solutions
    # extract actions
    action_status <- lapply(
      sol,
      function(x) matrix(x[[1]][seq_len(a$number_of_actions())] > 0.5, nrow = 1)
    )
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
    ### add statistics columns
    out <- tibble::as_tibble(cbind(out, solution_statistics(a, s)))
    ### reorder columns
    out <- out[, c(
      "solution", "status", "cost", "obj",
      a$action_names(), a$project_names(), a$feature_names()
    )]
    # return result
    out
  }
)

#' @name solve
#'
#' @rdname solve
methods::setMethod(
  "solve",
  signature(a = "MultiObjProjectProblem", b = "missing"),
  function(a, b, ...) {
    ## assertions
    assertthat::assert_that(
      !is.Waiver(a$approach),
      msg = "`a` must have a specified approach."
    )
    ## solve problem
    # assign solver
    if (inherits(a$solver, "Waiver")) {
      a <- add_default_solver(a)
    }
    # compile optimization problem
    opt <- lapply(a$problems, compile.ProjectProblem, ...)
    for (i in seq_along(opt)) {
      opt[[i]]$convert_pwlobj()
    }
    opt <- multi_compile(opt)
    # generate solution using approach
    a$approach$calculate(opt, a)
    sol <- a$approach$run(opt, a$solver)
    ## format solutions
    # identify feasible solutions
    sol_is_feasible <- vapply(sol, function(x) !is.null(x$x), logical(1))
    # check that at least solution is valid
    if (!any(sol_is_feasible)) {
      stop("project prioritization problem is infeasible")
    }
    # extract actions
    action_status <- t(vapply(
      sol[sol_is_feasible],
      function(x) x$x[seq_len(a$number_of_actions())] > 0.5,
      logical(a$number_of_actions())
    ))
    # create solution data
    ## initialize and add solution column
    out <- tibble::tibble(solution = which(sol_is_feasible))
    ## add status column
    out$status <- vapply(sol[sol_is_feasible], `[[`, character(1), 3)
    ## add solution columns
    s <- tibble::as_tibble(as.data.frame(action_status))
    names(s) <- a$action_names()
    out <- tibble::as_tibble(cbind(out, s))
    ## add statistics columns
    out <- tibble::as_tibble(cbind(out, solution_statistics(a, s)))
    ## reorder columns
    out <- out[, c(
      "solution", "status", "cost",
      a$problem_names(),
      a$action_names(),
      unlist(a$project_names(), recursive = TRUE, use.names = FALSE),
      unlist(a$feature_names(), recursive = TRUE, use.names = FALSE)
    )]
    # return result
    out
  }
)
