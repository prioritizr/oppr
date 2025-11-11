#' Represent options
#'
#' Return a pretty character representation of an object that contains
#' different options.
#'
#' @param x `object`.
#'
#' @param description `character` value with label.
#'
#' @return `character` object.
#'
#' @examples
#' repr_options(letters)
#' repr_options(letters, "characters")
#' @noRd
repr_options <- function(x, description = "") {
  n <- length(x)
  if (nchar(description) > 0) {
    description <- paste0(" ", description)
  }
  if (length(x) <= 4) {
    x <- x[seq_len(min(length(x), 4))]
  } else {
    x <- c(x[seq_len(min(length(x), 3))], "...")
  }
  paste0(paste(x, collapse = ", "), " (", n, description, ")")
}

#' Represent values
#'
#' Return a pretty character representation of an object with numeric values.
#'
#' @param x `object`.
#'
#' @return `character` object.
#'
#' @examples
#' repr_values(runif(10))
#' @noRd
repr_values <- function(x) {
  if (all(is.na(x))) {
    return("missing values (all are NA)")
  }
  x <- x[!is.na(x)]
  if (all(x %in% c(0, 1))) {
    return("binary values (all equal to zero or one)")
  }
  if (all(x >= 0 & x <= 1)) {
    return(
      paste0(
        "proportion values (between ",
        round(min(x), 3),
        " and ",
        round(max(x), 3),
        ")"
      )
    )
  }
  return(
    paste0(
      "continuous values (between ",
      round(min(x), 3),
      " and ",
      round(max(x), 3),
      ")"
    )
  )
}
