#' @include internal.R MultiObjProjectProblem-class.R ProjectProblem-class.R assertions.R
NULL

#' Multi-objective conservation planning problem
#'
#' Create a multi-objective systematic conservation planning problem.
#'
#' @param ... [problem()] objects.
#'
#' @param problem_names `character` vector with a name for each problem
#' in `...`. Defaults to `NULL`, such that the problem names are defined
#' automatically.
#'
#' @details
#' TODO.
#'
#' @seealso
#' TODO.
#'
#' @references
#' TODO.
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#' @export
multi_problem <- function(..., problem_names = NULL) {
  # parse arguments
  x <- list(...)

  # if needed, create default names
  if (is.null(names(x)) && is.null(problem_names)) {
    problem_names <- paste("Problem", seq_along(x))
  }

  # if need, assign names
  if (is.null(names(x)) && !is.null(problem_names)) {
    ## assert arguments are valid
    assertthat::assert_that(
      is.character(problem_names),
      assertthat::noNA(problem_names),
      length(unique(problem_names)) == 1
    )
    assertthat::assert_that(
      identical(length(problem_names), length(x)),
      msg = "`problem_names` must have a value each object in `...`."
    )
    ## assign names
    names(x) <- problem_names
  }

  # assert that arguments are valid
  assertthat::assert_that(
    length(x) >= 2,
    msg = "`...` must contain at least two `problem` objects."
  )
  assertthat::assert_that(
    all(vapply(x, inherits, FUN.VALUE = logical(1), "ProjectProblem")),
    msg = "`...` must contain only `problem` objects."
  )

  # assert objects are all ProjectProblem objects
  assertthat::assert_that(
    all(vapply(x, inherits, logical(1), "ProjectProblem")),
    msg = "`...` must contain `ProjectProblem` objects."
  )

  # assert that each object has exactly the same actions
  assertthat::assert_that(
    all(
      vapply(
        x, FUN.VALUE = logical(1),
        function(y) identical(x[[1]]$action_names(), y$action_names())
      )
    ),
    msg = "`...` must contain objects that all have exactly the same actions."
  )
  assertthat::assert_that(
    all(
      vapply(
        x, FUN.VALUE = logical(1),
        function(y) {
          identical(
            x[[1]]$data$actions[[x[[1]]$data$action_name_column]],
            y$data$actions[[y$data$action_name_column]]
          )
        }
      )
    ),
    msg = paste(
      "`...` must contain objects that all have exactly the same",
      "action names."
    )
  )
  assertthat::assert_that(
    all(
      vapply(
        x, FUN.VALUE = logical(1),
        function(y) {
          identical(
            x[[1]]$data$actions[[x[[1]]$data$action_cost_column]],
            y$data$actions[[y$data$action_cost_column]]
          )
        }
      )
    ),
    msg = paste(
      "`...` must contain objects that all have exactly the same",
      "action costs."
    )
  )

  # assert that each object has different features
  assertthat::assert_that(
    identical(
      anyDuplicated(
        unlist(
          lapply(x, function(y) y$feature_names()),
          recursive = FALSE, use.names = FALSE
        )
      ),
      0L
    ),
    msg = paste(
      "`...` must contain objects that all have different feature names."
    )
  )

  # assert that each object has different projects
  assertthat::assert_that(
    identical(
      anyDuplicated(
        unlist(
          lapply(x, function(y) y$project_names()),
          recursive = FALSE, use.names = FALSE
        )
      ),
      0L
    ),
    msg = paste(
      "`...` must contain objects that all have different project names."
    )
  )
  # if any of input problems have a non default solver specified, throw warning
  if (
    any(
      vapply(x, FUN.VALUE = logical(1), function(x) {
        !isTRUE(x$defaults$solver)
      })
    )
  ) {
    warning(
      "solvers specified for `...` input problems will be ignored.",
      immediate. = TRUE
    )
  }

  # if needed, set default problem names
  if (is.null(names(x))) {
    names(x) <- paste("Objective", seq_along(x)) # nocov
  }

  # create new multi objective conservation problem
  p <- MultiObjProjectProblem$new(problems = x)

  # add defaults
  p <- suppressWarnings(add_default_solver(p))
  p$defaults$solver <- TRUE

  # return result
  p
}
