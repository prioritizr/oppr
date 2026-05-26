#' Convert sparse matrix to triplet data.frame
#'
#' Convert a sparse matrix to a triplet `data.frame`.
#'
#' @param x `Matrix` object.
#'
#' @noRd
matrix_to_triplet_dataframe <- function(x) {
  if (inherits(x, c("dsCMatrix"))) {
    x <- methods::as(x, "dsTMatrix")
  }
  if (inherits(x, c("dgCMatrix", "matrix"))) {
    x <- methods::as(x, "dgTMatrix")
  }
  data.frame(i = x@i + 1, j = x@j + 1, x = x@x)
}

#' Convert a triplet data.frame to a matrix
#'
#' Convert a triplet data.framr object to a sparse matrix.
#'
#' @param x `data.frame` object. The first column contains the row
#'   numbers, the second column contains the column numbers, and the
#'   third column contains the cell values.
#
#' @return [Matrix::dgCMatrix-class] object.
#'
#' @noRd
triplet_dataframe_to_matrix <- function(x, forceSymmetric = FALSE, ...) {
  assertthat::assert_that(
    inherits(x, "data.frame"), isTRUE(ncol(x) == 3),
    isTRUE(all(x[[1]] == round(x[[1]]))), isTRUE(all(x[[2]] == round(x[[2]])))
  )
  # create sparse amtrix
  m <- Matrix::sparseMatrix(
    i = x[[1]], j = x[[2]], x = x[[3]],
    giveCsparse = FALSE, ...
  )
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
#' @param x `character` text.
#'
#' @param n `integer` number of spaces.
#'
#' @return `character`.
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
  if (!grepl("\n", x)) {
    return(x)
  }
  return(
    gsub(
      "\n", paste0("\n", paste(rep(" ", n), collapse = "")),
      x,
      fixed = TRUE
    )
  )
}

#' Any solvers installed?
#'
#' Test if any solvers are installed.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{Rsymphony}, \pkg{lpsymphony}, \pkg{gurobi}.
#'
#' @return `logical` value indicating if any solvers are installed.
#'
#' @noRd
any_solvers_installed <- function() {
  !is.null(default_solver_name())
}

#' Convert to Matrix
#'
#' Convert an object to a matrix class provided by the \pkg{Matrix} package.
#'
#' @param object object.
#'
#' @param class `character` name of new classes.
#'
#' @details
#' This function is a wrapper that is designed to provide
#' compatibility with older and newer versions of the \pkg{Matrix} package.
#'
#' @return `Matrix` object.
#'
#' @noRd
as_Matrix <- function(object, class) {
  # assert valid argument
  assertthat::assert_that(
    assertthat::is.string(class),
    assertthat::noNA(class)
  )
  # if we just want to convert to generic Matrix class then do that...
  if (identical(class, "Matrix")) {
    return(methods::as(object, class))
  }
  # convert matrix
  if (utils::packageVersion("Matrix") >= as.package_version("1.4-2")) {
    if (identical(class, "dgCMatrix")) {
      c1 <- "dMatrix"
      c2 <- "generalMatrix"
      c3 <- "CsparseMatrix"
    } else if (identical(class, "dgTMatrix")) {
      c1 <- "dMatrix"
      c2 <- "generalMatrix"
      c3 <- "TsparseMatrix"
    } else if (identical(class, "dsCMatrix")) {
      c1 <- "dMatrix"
      c2 <- "symmetricMatrix"
      c3 <- "CsparseMatrix"
    } else if (identical(class, "dsTMatrix")) {
      c1 <- "dMatrix"
      c2 <- "symmetricMatrix"
      c3 <- "TsparseMatrix"
    } else if (identical(class, "lgCMatrix")) {
      c1 <- "lMatrix"
      c2 <- "generalMatrix"
      c3 <- "CsparseMatrix"
    } else {
      stop("argument to \"class\" not recognized")
    }
    out <- methods::as(methods::as(methods::as(object, c1), c2), c3)
  } else {
    out <- methods::as(object, class)
  }
  # return result
  out
}
