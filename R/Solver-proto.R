#' @include internal.R pproto.R parameters.R
NULL

#' @export
if (!methods::isClass("Solver")) methods::setOldClass("Solver")
NULL

#' Solver prototype
#'
#' This prototype is used to generate objects that represent methods for solving
#' optimization problems. **This class represents a recipe to
#' create solver and and is only recommended for use by expert users. To
#' customize the method used to solve optimization problems, please see the
#' help page on [solvers]**.
#'
#' @section Fields:
#'
#' \describe{
#'
#' \item{$name}{`character` name of solver.}
#'
#' \item{$parameters}{`Parameters` object with parameters used to customize
#'   the the solver.}
#'
#' \item{$solve}{`function` used to solve a
#'   [OptimizationProblem-class] object.}
#' }
#'
#' @section Usage:
#'
#' `x$print()`
#'
#' `x$show()`
#'
#' `x$repr()`
#'
#' `x$solve(op)`
#'
#' @section Arguments:
#' \describe{
#'
#' \item{x}{[Solver-class] object.}
#'
#' \item{op}{[OptimizationProblem-class] object.}
#'
#' }
#'
#' @section Details:
#'
#' \describe{
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{`character` representation of object.}
#'
#' \item{solve}{solve an [OptimizationProblem-class] using this
#'   object.}
#'
#' }
#'
#' @name Solver-class
#'
#' @aliases Solver
NULL

#' @export
Solver <- pproto(
  "Solver",
  name = character(0),
  solve = function(...) stop("solver is missing a solve method"),
  parameters = parameters(),
  print = function(self) {
    message(self$repr())
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    paste(self$name, self$parameters$repr())
  },
  get_paramter = function(self, x) {
    self$parameters$get(x)
  },
  set_parameter = function(self, value) {
    self$parameters$set(x, value)
  },
  render_parameter = function(self, x) {
    self$parameters$render(x)
  },
  render_all_parameters = function(self) {
    shiny::div(class = "Solver",
                self$parameters$render_all())
  })
