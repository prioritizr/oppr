
#' @include internal.R
NULL

#' Compile a multi-objective optimization problem
#'
#' Compile multiple [`OptimizationProblem-class`] objects for
#' multi-objective optimization.
#'
#' @param x [multi_problem()] or `list` of [`OptimizationProblem-class`]
#' objects.
#'
#' @param ... arguments passed to [compile()].
#' @return
#' A `list` containing a (`$obj`) `numeric` matrix with the coefficients
#' for each of the objectives (i.e., rows correspond to different
#' objectives and columns correspond to different decision variables),
#' (`$modelsense`) `character` vector indicating if each
#' objective should be maximized or minimized
#' (i.e., each element corresponds to a different objective),
#' and a (`$opt`) [`OptimizationProblem-class`] object with all
#' of the constraints present in `x` (note that the objective coefficients
#' for this object are all zero).
#'
#' @seealso
#' See [compile()] to create an [`OptimizationProblem-class`] object.
#'
#' @examples
#' # load data
#' data(sim_multi_projects)
#' data(sim_multi_features)
#' data(sim_multi_actions)
#' data(sim_multi_tree)
#'
#' # build problem
#' p <-
#'   multi_problem(
#'     obj1 =
#'       problem(
#'         sim_multi_projects[[1]], sim_multi_actions, sim_multi_features[[1]],
#'         "name", "success", "name", "cost", "name",
#'         baseline_project_name = "baseline_project_obj1"
#'       ) %>%
#'       add_max_phylo_div_objective(
#'        budget = 200, tree = sim_multi_tree[[1]]
#'       ) %>%
#'       add_binary_decisions(),
#'    obj2 =
#'      problem(
#'        sim_multi_projects[[2]], sim_multi_actions, sim_multi_features[[2]],
#'        "name", "success", "name", "cost", "name",
#'        baseline_project_name = "baseline_project_obj2"
#'      ) %>%
#'      add_max_richness_objective(budget = 200) %>%
#'      add_binary_decisions(),
#'
#' # compile problem
#' o <- multi_compile(p)
#'
#' # print object
#' print(o)
#' @noRd
multi_compile <- function(x, ...) {
  UseMethod("multi_compile")
}

#' @rdname compile
#' @export
multi_compile.MultiObjProjectProblem <- function(x, ...) {
  # compile each problem individually, and compile into multi-objective problem
  multi_compile.list(
    stats::setNames(
      lapply(x$problems, compile.ProjectProblem),
      names(x$problems)
    )
  )
}

#' @rdname compile
#' @export
multi_compile.list <- function(x, ...) {
  # assert arguments are valid
  assertthat::assert_that(is.list(x))
  assertthat::assert_that(
    all(vapply(x, inherits, logical(1), "OptimizationProblem")),
    msg = "`x` must only contain `OptimizationProblem` objects."
  )
  assertthat::assert_that(
    all(
      vapply(
        x, FUN.VALUE = logical(1),
        function(x) identical(length(x$pwlobj()), 0L)
      )
    ),
    msg = paste(
      "`x` must not contain any `OptimizationProblem` objects with",
      "piece-wise linear components."
    )
  )

  # compile multi-objective optimization problem
  out <- rcpp_compile_multi_obj_problem(lapply(x, function(z) z$ptr))

  # if needed, set names for objectives
  if (!is.null(names(x))) {
    rownames(out$obj) <- names(x)
  }

  # convert pointer to optimization problem object
  out$opt <- OptimizationProblem$new(ptr = out$opt)

  # return result
  out
}
