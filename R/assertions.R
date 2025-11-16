#' No extra arguments
#'
#' Check that no additional unused arguments have been supplied to a function
#' through the `...`.
#'
#' @param ... arguments that are not used.
#'
#' @return A `logical` value indicating success.
#'
#' @noRd
no_extra_arguments <- function(...) {
  return(length(list(...)) == 0)
}

assertthat::on_failure(no_extra_arguments) <- function(call, env) {
  call_list <- as.list(call)[-1]
  format_args <- function(i) {
    if (names(call_list)[i] == "") {
      return(deparse(call_list[[i]]))
    }
    paste0(names(call_list)[i], "=", deparse(call_list[[i]]))
  }
  msg <- paste(
    vapply(seq_along(call_list), format_args, character(1)),
    collapse = ", "
  )
  if (length(call_list) > 1) {
    msg <- paste0("unused arguments (", msg, ")")
  } else {
    msg <- paste0("unused argument (", msg, ")")
  }
  msg
}

#' Is valid phylogeny?
#'
#' Check that a phylogeny is valid.
#'
#' @param x object.
#'
#' @return A `logical` value indicating success.
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
