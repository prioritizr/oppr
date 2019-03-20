#' Check
#'
#' Check that the output from \code{\link[assert_that]{see_if}}
#' is valid.
#'
#' @param x \code{character} or \code{object}
#'
#' @details This object will return an error if the argument to \code{x}
#'   is \code{FALSE}, and for the error message, will show the error
#'   message attached to the object.
#'
#' @return invisible \code{TRUE}.
#'
#' @noRd
check_that <- function(x) {
  if (!isTRUE(x))
    stop(attr(x, "msg")[1])
  invisible(TRUE)
}

#' Convert sparse matrix to triplet data.frame
#'
#' Convert a sparse matrix to a triplet \code{data.frame}.
#'
#' @param x \code{Matrix} object.
#'
#' @noRd
matrix_to_triplet_dataframe <- function(x) {
  if (inherits(x, c("dsCMatrix")))
    x <- methods::as(x, "dsTMatrix")
  if (inherits(x, c("dgCMatrix", "matrix")))
    x <- methods::as(x, "dgTMatrix")
  data.frame(i = x@i + 1, j = x@j + 1, x = x@x)
}

#' Convert a triplet data.frame to a matrix
#'
#' Convert a triplet data.framr object to a sparse matrix.
#'
#' @param x \code{data.frame} object. The first column contains the row
#'   numbers, the second column contains the column numbers, and the
#'   third column contains the cell values.
#
#' @return \code{\link[Matrix]{dgCMatrix-class}} object.
#'
#' @noRd
triplet_dataframe_to_matrix <- function(x, forceSymmetric=FALSE, ...) {
  assertthat::assert_that(inherits(x, "data.frame"), isTRUE(ncol(x) == 3),
    isTRUE(all(x[[1]] == round(x[[1]]))), isTRUE(all(x[[2]] == round(x[[2]]))))
  # create sparse amtrix
  m <- Matrix::sparseMatrix(i = x[[1]], j = x[[2]], x = x[[3]],
                            giveCsparse = FALSE, ...)
  if (forceSymmetric) {
    # force the matrix to be symmetric
    # we cannot gurantee that the cells that are filled in belong to either
    # the upper or the lower diagonal
    m2 <- matrix(c(m@j + 1, m@i + 1, m@x), ncol = 3)
    m2 <- m2[m2[, 1] != m2[, 2], ]
    m[m2[, 1:2]] <- m2[, 3]
    return(Matrix::forceSymmetric(m))
  } else {
    # return matrix in compressed format
    return(methods::as(m, "dgCMatrix"))
  }
}

#' Align text
#'
#' Format text by adding a certain number of spaces after new line characters.
#'
#' @param x \code{character} text.
#'
#' @param n \code{integer} number of spaces.
#'
#' @return \code{character}.
#'
#' @examples
#' # make some text
#' original_text <- "animals: horse\npig\nbear"
#'
#' # print text
#' message(original_text)
#'
#' # this look really ugly so we will align it
#' aligned_text <- align_text(original_text, 9)
#'
#' # print aligned text
#' message(aligned_text)
#'
#' @noRd
align_text <- function(x, n) {
  assertthat::assert_that(assertthat::is.string(x), assertthat::is.count(n))
  if (!grepl("\n", x))
    return(x)
  return(gsub("\n", paste0("\n", paste(rep(" ", n), collapse = "")), x,
              fixed = TRUE))
}

#' Atomic representation
#'
#' Return a pretty character representation of an object with elements and
#  names.
#'
#' @param x \code{object}.
#'
#' @return \code{character} object.
#'
#' @examples
#' repr_atomic(letters)
#' repr_atomic(letters, "characters")
#' @noRd
repr_atomic <- function(x, description = "") {
  n <- length(x)
  if (nchar(description) > 0)
    description <- paste0(" ", description)
  if (length(x) <= 4) {
    x <- x[seq_len(min(length(x), 4))]
  } else {
    x <- c(x[seq_len(min(length(x), 3))], "...")
  }
  paste0(paste(x, collapse = ", "), " (", n, description, ")")
}

#' No extra arguments
#'
#' Check that no additional unused arguments have been supplied to a function
#' through the \code{...}.
#'
#' @param ... arguments that are not used.
#'
#' @return \code{logical} indicating success.
#'
#' @noRd
no_extra_arguments <- function(...) {
  return(length(list(...)) == 0)
}

assertthat::on_failure(no_extra_arguments) <- function(call, env) {
  call_list <- as.list(call)[-1]
  format_args <- function(i) {
    if (names(call_list)[i] == "")
     return(deparse(call_list[[i]]))
    paste0(names(call_list)[i], "=", deparse(call_list[[i]]))
  }
  msg <- paste(vapply(seq_along(call_list), format_args, character(1)),
               collapse = ", ")
  if (length(call_list) > 1) {
    msg <- paste0("unused arguments (", msg, ")")
  } else {
    msg <- paste0("unused argument (", msg, ")")
  }
  msg
}

#' Verify if assertion is met
#'
#' Verify if an assertion is tmet and throw a \code{\link{warning}} if it
#' is not. This function is equivalent to \code{\link[assertthat]{assert_that}}
#' except that it throws warnings and not errors.
#'
#' @param x \code{logical} condition.
#'
#' @return \code{logical} if assertion is met and a \code{warning} if it is not.
#'
#' @noRd
verify_that <- function(..., env = parent.frame()) {
  res <- assertthat::validate_that(..., env = env)
  if (isTRUE(res))
      return(TRUE)
  warning(res)
  FALSE
}

#' Any solvers installed?
#'
#' Test if any solvers are installed.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{Rsymphony}, \pkg{lpsymphony}, \pkg{gurobi}.
#'
#' @return \code{logical} value indicating if any solvers are installed.
#'
#' @noRd
any_solvers_installed <- function() {
  !is.null(default_solver_name())
}

#' Default solver name
#'
#' This function returns the name of the default solver. If no solvers are
#' detected on the system, then a \code{NULL} object is returned.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{Rsymphony}, \pkg{lpsymphony}, \pkg{gurobi}.
#'
#' @return \code{character} indicating the name of the default solver.
#'
#' @noRd
default_solver_name <- function() {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return("gurobi")
  } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
    return("Rsymphony")
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return("lpsymphony")
  } else if (requireNamespace("lpSolveAPI", quietly = TRUE)) {
    return("lpSolveAPI")
  } else {
    return(NULL)
  }
}

#' Is valid phylogeny?
#'
#' Check that a phylogeny is valid.
#'
#' @param x object.
#'
#' @return \code{logical} indicating success.
#'
#' @noRd
is_valid_phylo <- function(x) {
    msg <- utils::capture.output(ape::checkValidPhylo(x))
    !((any(grepl("FATAL", msg)) || any(grepl("MODERATE", msg))))
}

assertthat::on_failure(is_valid_phylo) <- function(call, env) {
  x <- eval(as.list(call)$x, env)
  msg <- utils::capture.output(ape::checkValidPhylo(x))
  paste(msg, collapse = "\n")
}


#' SYMPHONY status
#'
#' Find a description of the solver status returned from SYMPHONY.
#'
#' @param x \code{numeric} status code.
#'
#' @return \code{character} status description.
#'
#' @noRd
symphony_status <- function(x) {
  assertthat::assert_that(is.numeric(x))
  codes <- c(
    "0" = "TM_OPTIMAL_SOLUTION_FOUND",
    "225" = "TM_NO_PROBLEM",
    "226" = "TM_NO_SOLUTION",
    "227" = "TM_OPTIMAL_SOLUTION_FOUND",
    "228" = "TM_TIME_LIMIT_EXCEEDED",
    "229" = "TM_NODE_LIMIT_EXCEEDED",
    "230" = "TM_ITERATION_LIMIT_EXCEEDED",
    "231" = "TM_TARGET_GAP_ACHIEVED",
    "232" = "TM_FOUND_FIRST_FEASIBLE",
    "233" = "TM_FINISHED",
    "234" = "TM_UNFINISHED",
    "235" = "TM_FEASIBLE_SOLUTION_FOUND",
    "236" = "TM_SIGNAL_CAUGHT",
    "237" = "TM_UNBOUNDED",
    "238" = "PREP_OPTIMAL_SOLUTION_FOUND",
    "239" = "PREP_NO_SOLUTION",
    "-250" = "TM_ERROR__NO_BRANCHING_CANDIDATE",
    "-251" = "TM_ERROR__ILLEGAL_RETURN_CODE",
    "-252" = "TM_ERROR__NUMERICAL_INSTABILITY",
    "-253" = "TM_ERROR__COMM_ERROR",
    "-275" = "TM_ERROR__USER",
    "-276" = "PREP_ERROR")
  x <- codes[as.character(x)]
  if (is.na(x))
    warning("solver returned unrecognized code")
  as.character(x)
}

#' lp_solve status
#'
#' Find a description of the solver status returned from lp_solve.
#'
#' @param x \code{numeric} status code.
#'
#' @return \code{character} status description.
#'
#' @noRd
lp_solve_status <- function(x) {
  assertthat::assert_that(is.numeric(x))
  codes <- c(
    "0" = "optimal solution found",
    "1" ="the model is sub-optimal",
    "3" ="the model is unbounded",
    "2" ="the model is infeasible",
    "4" ="the model is degenerate",
    "5" ="numerical failure encountered",
    "6" ="process aborted",
    "7" ="timeout",
    "9" ="the model was solved by presolve",
    "10" ="the branch and bound routine failed",
    "11" ="the branch and bound was stopped because of a break-at-first or break-at-value",
    "12" = "a feasible branch and bound solution was found",
    "13" = "no feasible branch and bound solution was found")
  x <- codes[as.character(x)]
  if (is.na(x))
    warning("solver returned unrecognized code")
  as.character(x)
}
