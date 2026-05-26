#' @include Solver-class.R
NULL

#' Add a *HiGHS* solver
#'
#' Add a solver to generate solutions to a project prioritization problem
#' with the [*HiGHS*](https://highs.dev/) software (Huangfu and Hall 2018).
#' This function can also be used to customize the behavior of the solver.
#' It requires the \pkg{highs} package to be installed.
#'
#' @inheritParams add_gurobi_solver
#'
#' @param control `list` with additional parameters for tuning
#'  the optimization process.
#'  For example, `control = list(simplex_strategy = 1)` could be used to
#'  set the `simplex_strategy` parameter.
#'  See the [online documentation](https://ergo-code.github.io/HiGHS/dev/options/definitions/)
#'  for information on the parameters.
#'
#' @inherit add_gurobi_solver return seealso
#'
#' @family solvers
#'
#' @references
#' Huangfu Q and Hall JAJ (2018). Parallelizing the dual revised simplex
#' method. *Mathematical Programming Computation*, 10: 119-142.
#'
#' @examplesIf oppr::run_example()
#' # load data
#' data(sim_projects, sim_features, sim_actions)
#'
#' # build problem with highs solver
#' p <-
#'   problem(
#'     sim_projects, sim_actions, sim_features,
#'     "name", "success", "name", "cost", "name"
#'   ) %>%
#'   add_max_wtd_sum_objective(budget = 200) %>%
#'   add_binary_decisions() %>%
#'   add_highs_solver()
#'
#' # print problem
#' print(p)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # print solution
#' print(s)
#'
#' # plot solution
#' plot(p, s)
#' @name add_highs_solver
NULL

#' @rdname add_highs_solver
#' @export
add_highs_solver <- function(x, gap = 0, time_limit = .Machine$integer.max,
                             presolve = TRUE, threads = 1,
                             start = NULL,
                             verbose = TRUE,
                             control = list()) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, c("ProjectProblem", "MultiObjProjectProblem")),
    assertthat::is.number(gap),
    assertthat::noNA(gap),
    gap >= 0,
    assertthat::is.count(time_limit),
    assertthat::noNA(time_limit),
    assertthat::is.flag(presolve),
    assertthat::noNA(presolve),
    assertthat::is.count(threads),
    assertthat::noNA(threads),
    assertthat::is.flag(verbose),
    is.list(control),
    requireNamespace("highs", quietly = TRUE)
  )
  if (!is.null(start)) {
    assertthat::assert_that(
      is.logical(start),
      length(start) == x$number_of_actions()
    )
    start <- as.numeric(start)
  }
  # additional checks for control
  if (length(control) > 0) {
    assertthat::assert_that(
      !is.null(names(control)),
      all(nzchar(names(control))),
      msg = "all elements in `control` must have a name."
    )
  }
  # add solver
  x$add_solver(
    R6::R6Class(
      "HighsSolver",
      inherit = Solver,
      public = list(
        name = "highs solver",
        data = list(
          gap = gap,
          time_limit = time_limit,
          presolve = presolve,
          threads = threads,
          start = start,
          verbose = verbose,
          control = control
        ),
        solve = function(x, ...) {
          # prepare constraints
          ## extract info
          rhs <- x$rhs()
          sense <- x$sense()
          ## initialize arguments
          row_lb <- numeric(length(rhs))
          row_ub <- numeric(length(rhs))
          ## set equality constraints
          idx <- which(sense == "=")
          row_lb[idx] <- rhs[idx]
          row_ub[idx] <- rhs[idx]
          ## set lte constraints
          idx <- which(sense == "<=")
          row_lb[idx] <- -Inf
          row_ub[idx] <- rhs[idx]
          ## set gte constraints
          idx <- which(sense == ">=")
          row_lb[idx] <- rhs[idx]
          row_ub[idx] <- Inf
          # create problem
          model <- list(
            maximum = identical(x$modelsense(), "max"),
            L = x$obj(),
            A = x$A(),
            lhs = row_lb,
            rhs = row_ub,
            types = x$vtype(),
            lower = x$lb(),
            upper = x$ub()
          )
          # round values < 1e-6 to zero and drop them
          model$A@x[abs(model$A@x) < 1e-6] <- 0
          model$A <- Matrix::drop0(model$A)
          model$L[abs(model$L) < 1e-6] <- 0
          # set variables types
          ## C = continuous (same as gurobi)
          ## I = integer (same as gurobi)
          ## SC = semi-continuous (not same as gurobi)
          ## binary type not supported, convert to integer gurobi)
          model$types[model$types == "B"] <- "I"
          model$types[model$types == "S"] <- "SC"
          # set starting solution
          ## note this functionality is only supported by developmental
          ## versions of highs
          if ("start" %in% names(formals(highs::highs_solve))) {
            start <- self$get_data("start")
            if (!is.null(start) && !is.Waiver(start) && is.numeric(start)) {
              n_extra <- max(length(model$L) - length(start), 0)
              model$start <- c(c(start), rep(NA_real_, n_extra))
            }
          }
          # create parameters
          p <- list(
            log_to_console = self$get_data("verbose"),
            presolve = ifelse(self$get_data("presolve") > 0.5, "on", "off"),
            mip_rel_gap = self$get_data("gap"),
            time_limit = as.numeric(self$get_data("time_limit")),
            threads = self$get_data("threads")
          )
          # specify custom parameters
          control <- self$get_data("control")
          if (length(control) > 0) {
            p[names(control)] <- control
          }
          # solve problem
          rt <- system.time({
            x <- do.call(
              highs::highs_solve,
              append(model, list(control = do.call(highs::highs_control, p)))
            )
          })
          # manually return NULL to indicate error if no solution
          # nocov start
          if (
            is.null(x) ||
            is.null(x$primal_solution) ||
            any(is.na(x$primal_solution)) ||
            isTRUE(!x$status %in% c(7L, 11L, 12L, 13L, 14L))
          ) {
            return(NULL)
          }
          # nocov end
          # extract solution values
          sol <- x$primal_solution
          ## fix potential floating point arithmetic issues
          i <- model$types == "I"
          if (is.numeric(sol)) {
            ## round integer variables
            sol[i] <- round(sol[i])
            ## truncate variables to account for rounding issues
            sol <- pmax(sol, model$lower)
            sol <- pmin(sol, model$upper)
          }
          # extract optimality gap
          if (!is.null(x$info) && !is.null(x$info$mip_gap)) {
            x_gap <- x$info$mip_gap
          } else {
            x_gap <- NA_real_ # nocov
          }
          # return solution
          list(
            list(
              x = sol,
              objective = x$objective_value,
              status = x$status_message,
              runtime = rt[[3]],
              gap = x_gap
            )
          )
        }
      )
    )$new()
  )
}
