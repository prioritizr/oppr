#' @include internal.R
NULL

#' Deprecation notice
#'
#' The functions listed here are deprecated.
#' This means that they once existed in earlier versions of the
#' of the \pkg{oppr} package, but they have since been removed
#' entirely, replaced by other functions, or renamed as other functions
#' in newer versions.
#' To help make it easier to transition to new versions of the \pkg{oppr}
#' package, we have listed alternatives for deprecated the functions
#' (where applicable).
#' If a function is described as being renamed, then this means
#' that only the name of the function has changed
#' (i.e., the inputs, outputs, and underlying code remain the same).
#'
#' @param ... not used.
#'
#' @details
#' The following functions have been deprecated:
#'
#' \describe{
#'
#' \item{`add_max_richness_objective()`}{renamed
#'   as the [add_max_wtd_sum_objective()] function.}
#'
#' \item{`plot_feature_persistence()`}{renamed
#'   as the [plot_solution_barplot()] function.}
#'
#' \item{`plot_phylo_persistence()`}{renamed
#'   as the [plot_solution_phylogram()] function.}
#'
#' \item{`add_locked_in_constraints()`}{renamed
#'   as the [add_locked_in_action_constraints()] function.}
#'
#' \item{`add_locked_out_constraints()`}{renamed
#'   as the [add_locked_out_action_constraints()] function.}
#'
#' \item{`add_manual_locked_constraints()`}{renamed
#'   as the [add_manual_locked_action_constraints()] function.}
#'
#' }
#'
#' @keywords deprecated
#'
#' @name oppr-deprecated
NULL

#' @rdname oppr-deprecated
#' @export
plot_feature_persistence <- function(...) {
  error_defunct(
    old = "plot_feature_persistence",
    new = "plot_solution_barplot"
  )
}

#' @rdname oppr-deprecated
#' @export
plot_phylo_persistence <- function(...) {
  error_defunct(
    old = "plot_phylo_persistence",
    new = "plot_solution_phylogram"
  )
}

#' @rdname oppr-deprecated
#' @export
add_locked_in_constraints <- function(...) {
  error_defunct(
    old = "add_locked_in_constraints",
    new = "add_locked_in_action_constraints"
  )
}

#' @rdname oppr-deprecated
#' @export
add_locked_out_constraints <- function(...) {
  error_defunct(
    old = "add_locked_out_constraints",
    new = "add_locked_out_action_constraints"
  )
}

#' @rdname oppr-deprecated
#' @export
add_manual_locked_constraints <- function(...) {
  error_defunct(
    old = "add_manual_locked_constraints",
    new = "add_manual_locked_action_constraints"
  )
}

#' Defunct
#'
#' Throw an error indicating that a function is defunct.
#'
#' @param old `character` value with name of defunct function.
#'
#' @param new `character` value with name of replacement function.
#'   Defaults to `NULL` such that no replacement function is detailed.
#'
#' @return None.
#'
#' @examples
#' error_defunct(old = "greg", new = "gred")
#'
#' @noRd
error_defunct <- function(old, new) {
  stop(
    paste0("`", old, "()` is defunct. Use `", new, "()` instead."),
    call. = FALSE
  )
}
