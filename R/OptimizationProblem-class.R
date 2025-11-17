#' @include internal.R
NULL

#' @export
if (!methods::isClass("OptimizationProblem")) methods::setOldClass("OptimizationProblem")
NULL

#' Optimization problem class
#'
#' @description
#' This class is used to represent an optimization problem.
#' It stores the information needed to generate a solution using
#' an exact algorithm solver.
#' Most users should use [compile()] to generate new optimization problem
#' objects, and the functions distributed with the package to interact
#' with them (e.g., [base::as.list()]).
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name OptimizationProblem-class
#'
#' @family classes
#'
#' @export
OptimizationProblem <- R6::R6Class(
  "OptimizationProblem",
  public = list(
    #' @field ptr A `Rcpp::Xptr` external pointer.
    ptr = NULL,

    #' @field data A `list` with supplemental data.
    data = NULL,

    #' Create a new optimization problem object.
    #' @param ptr `Rcpp::Xptr` external pointer.
    #' @param data `list` with supplemental data.
    #' @return A new `OptimizationProblem` object.
    initialize = function(ptr, data = list()) {
      self$ptr <- ptr
      self$data <- data
    },

    #' @description
    #' Obtain the supplemental data.
    #' @return A `list` object.
    get_data = function() {
      self$data
    },

    #' @description
    #' Print concise information about the object.
    #' @return Invisible `TRUE`.
    print = function() {
      if (self$ncol() > 0) {
        cv <- table(self$vtype())
        cv <- paste(paste(unname(cv), paste0("(", names(cv), ")")),
          collapse = ", "
        )
        message(
          "optimization problem",
          "\n  objective:   ", ifelse(length(self$pwlobj()) == 0, "linear",
            "piece-wise linear"
          ),
          "\n  model sense: ", self$modelsense(),
          "\n  dimensions:  ", self$nrow(), ", ", self$ncol(), ", ", self$ncell(),
          " (nrow, ncol, ncell)",
          "\n  variables:   ", cv
        )
      } else {
        message("optimization problem (empty)")
      }
      invisible(TRUE)
    },

    #' @description
    #' Print concise information about the object.
    #' @return Invisible `TRUE`.
    show = function() {
      self$print()
    },

    #' @description
    #' Obtain the number of columns in the problem formulation.
    #' @return A `numeric` value.
    ncol = function() {
      rcpp_get_optimization_problem_ncol(self$ptr)
    },

    #' @description
    #' Obtain the number of rows in the problem formulation.
    #' @return A `numeric` value.
    nrow = function() {
      rcpp_get_optimization_problem_nrow(self$ptr)
    },

    #' @description
    #' Obtain the number of cells in the problem formulation.
    #' @return A `numeric` value.
    ncell = function() {
      rcpp_get_optimization_problem_ncell(self$ptr)
    },

    #' @description
    #' Obtain the model sense.
    #' @return A `character` value.
    modelsense = function() {
      rcpp_get_optimization_problem_modelsense(self$ptr)
    },

    #' @description
    #' Obtain the decision variable types.
    #' @return A `character` vector.
    vtype = function() {
      rcpp_get_optimization_problem_vtype(self$ptr)
    },

    #' @description
    #' Obtain the objective function.
    #' @return A `numeric` vector.
    obj = function() {
      rcpp_get_optimization_problem_obj(self$ptr)
    },

    #' @description
    #' Obtain the piecewise linear components of the objective function.
    #' @return A `list` object.
    pwlobj = function() {
      rcpp_get_optimization_problem_pwlobj(self$ptr)
    },

    #' @description
    #' Obtain the constraint matrix.
    #' @return A [Matrix::sparseMatrix()] object.
    A = function() {
      x <- rcpp_get_optimization_problem_A(self$ptr)
      Matrix::sparseMatrix(
        i = x$i, j = x$j, x = x$x, index1 = FALSE,
        giveCsparse = TRUE,
        dims = c(self$nrow(), self$ncol())
      )
    },

    #' @description
    #' Obtain the right-hand-side constraint values.
    #' @return A `numeric` vector.
    rhs = function() {
      rcpp_get_optimization_problem_rhs(self$ptr)
    },

    #' @description
    #' Obtain the constraint senses.
    #' @return A `character` vector.
    sense = function() {
      rcpp_get_optimization_problem_sense(self$ptr)
    },

    #' @description
    #' Obtain the lower bounds for the decision variables.
    #' @return A `numeric` vector.
    lb = function() {
      rcpp_get_optimization_problem_lb(self$ptr)
    },

    #' @description
    #' Obtain the upper bounds for the decision variables.
    #' @return A `numeric` vector.
    ub = function() {
      rcpp_get_optimization_problem_ub(self$ptr)
    },

    #' @description
    #' Obtain the number of features.
    #' @return A `numeric` value.
    number_of_features = function() {
      rcpp_get_optimization_problem_number_of_features(self$ptr)
    },

    #' @description
    #' Obtain the number of phylogenetic branches.
    #' @return A `numeric` value.
    number_of_branches = function() {
      rcpp_get_optimization_problem_number_of_branches(self$ptr)
    },

    #' @description
    #' Obtain the number of allocation variables. This number represents
    #' the total number of decision variables used to identify if
    #' each project is allocated to each variable.
    #' @return A `numeric` value.
    number_of_allocations = function() {
      rcpp_get_optimization_problem_number_of_allocations(self$ptr);
    },

    #' @description
    #' Obtain the number of actions
    #' @return A `numeric` value.
    number_of_actions = function() {
      rcpp_get_optimization_problem_number_of_actions(self$ptr)
    },

    #' @description
    #' Obtain the number of projects.
    #' @return A `numeric` value.
    number_of_projects = function() {
      rcpp_get_optimization_problem_number_of_projects(self$ptr)
    },

    #' @description
    #' Obtain the identifiers for the columns.
    #' @return A `character` value.
    col_ids = function() {
      rcpp_get_optimization_problem_col_ids(self$ptr)
    },

    #' @description
    #' Obtain the identifiers for the rows.
    #' @return A `character` value.
    row_ids = function() {
      rcpp_get_optimization_problem_row_ids(self$ptr)
    },

    #' @description
    #' Copy the object.
    #' @return An `OptimizationProblem` object.
    copy = function() {
      OptimizationProblem$new(
        ptr = rcpp_copy_optimization_problem(self$ptr),
        data = self$data
      )
    },

    #' @description
    #' Convert the piece-wise linear components of the objective function
    #' into linear objective components and constraints.
    #' @return An invisible `TRUE`.
    convert_pwlobj = function() {
      rcpp_convert_pwlobj(self$ptr)
      invisible(TRUE)
    }
  )
)
