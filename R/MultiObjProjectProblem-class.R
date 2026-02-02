#' @include internal.R waiver.R
NULL

#' @export
if (!methods::isClass("MultiObjProjectProblem")) methods::setOldClass("MultiObjProjectProblem")
NULL

#' Multi-objective project problem class
#'
#' @description
#' This class is used to represent multi-objective project planning
#' problems. It stores the data (e.g., actions, and features) and
#' mathematical formulation (e.g., the objective, constraints,
#' and other design criteria) needed to generate prioritizations.
#' Most users should use [multi_problem()] to generate new
#' multi-objective project problem objects, and the functions distributed
#' with the package to interact
#' with them.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name MultiObjProjectProblem-class
#'
#' @family classes
#'
#' @export
MultiObjProjectProblem <- R6::R6Class(
  "MultiObjProjectProblem",
  public = list(

    #' @field problems `list` containing [`ProjectProblem-class`] objects.
    problems = list(),

    #' @field defaults `list` indicating if other fields contain defaults.
    defaults = list(
      approach = TRUE,
      solver = TRUE
    ),

    #' @field approach [`MultiObjApproach-class`] object
    #' for specifying the multi-objective optimization approach.
    approach = new_waiver(),

    #' @field solver [`Solver-class`] object specifying the solver for
    #' generating solutions.
    solver = new_waiver(),

    #' @description
    #' Create a new multi-objective conservation problem object.
    #' @param problems `list` containing [`ProjectProblem-class`] objects.
    #' @return A new `MultiObjProjectProblem` object.
    initialize = function(problems) {
      self$problems <- problems
    },

    #' @description
    #' Print concise information about the object.
    #' @return Invisible `TRUE`.
    print = function() {
      # specify text to display
      ## prepare text for constituent problems
      problem_msgs <- vapply(
        seq_along(self$problems), FUN.VALUE = character(1), function(i) {
          ### prepare text each component for i'th constituent of problem
          obj_msg <- "none specified"
          if (!isTRUE(self$problems[[i]]$defaults$objective)) {
            obj_msg <- self$problems[[i]]$objective$repr()
          }
          targets_msg <- "none specified"
          if (!isTRUE(self$problems[[i]]$defaults$targets)) {
            targets_msg <- self$problems[[i]]$targets$repr()
          }
          weights_msg <- "none specified"
          if (!isTRUE(self$problems[[i]]$defaults$weights)) {
            weights_msg <- self$problems[[i]]$weights$repr()
          }
          decisions_msg <- "none specified"
          if (!isTRUE(self$problems[[i]]$defaults$decisions)) {
            decisions_msg <- self$problems[[i]]$decisions$repr()
          }
          constraints_msg <- "none specified"
          if (!isTRUE(self$problems[[i]]$defaults$constraints)) {
            constraints_msg <- paste(
              vapply(
                self$problems[[i]]$constraints,
                function(x) x$repr(),
                character(1)
              ),
              collapse = ", "
            )
          }
          ### prepare text for x'th constituent of problem
          paste0(
            "\nobjective:         ", self$problem_names()[[i]],
            "\n  projects:        ",
            repr_options(self$problems[[i]]$project_names(), "projects"),
            "\n  features:        ",
            repr_options(self$problems[[i]]$feature_names(), "features"),
            "\n  project success: ",
            repr_values(self$problems[[i]]$project_success_probabilities()),
            "\n  objective:       ", obj_msg,
            "\n  targets:         ", targets_msg,
            "\n  weights:         ", weights_msg,
            "\n  constraints:     ", constraints_msg,
            "\n  decisions:       ", decisions_msg
          )
        }
      )
      ## prepare text for other components
      approach_msg <- "none specified"
      if (!isTRUE(self$defaults$approach)) {
        approach_msg <- self$approach$repr()
      }
      solver_msg <- "none specified"
      if (!isTRUE(self$defaults$solver)) {
        solver_msg <- self$solver$repr()
      }
      # display message
      message(
        paste0(
          "Multi-objective Project Prioritization Problem",
          paste(problem_msgs, collapse = ""),
          "\nactions:           ",
          repr_options(self$action_names(), "actions"),
          "\naction costs:      ",
          repr_values(self$problems[[1]]$action_costs()),
          "\napproach:          ", approach_msg,
          "\nsolver:            ", solver_msg
        )
      )
      # return success
      invisible(TRUE)
    },

    #' @description
    #' Display concise information about the object.
    #' @return Invisible `TRUE`.
    show = function() {
      self$print()
      invisible(TRUE)
    },


    #' @description
    #' Generate a character representation of the object.
    #' @return A `character` value.
    repr = function() {
      "MultiObjProjectProblem object"
    },

    #' @description
    #' Obtain the number of problems.
    #' @return An `integer` value.
    number_of_problems = function() {
      length(self$problems)
    },

    #' @description
    #' Obtain the number of features.
    #' @return An `integer` value.
    number_of_features = function() {
      sum(
        vapply(
          self$problems,
          FUN.VALUE = numeric(1),
          function(x) x$number_of_features()
        )
      )
    },

    #' @description
    #' Obtain the number of actions.
    #' @return An `integer` value.
    number_of_actions = function() {
      self$problems[[1]]$number_of_actions()
    },

    #' @description
    #' Obtain the number of projects.
    #' @return An `integer` value.
    number_of_projects = function() {
      sum(
        vapply(
          self$problems,
          FUN.VALUE = numeric(1),
          function(x) x$number_of_projects()
        )
      )
    },

    #' @description
    #' Obtain the names of the problems.
    #' @return An `character` value.
    problem_names = function() {
      names(self$problems)
    },

    #' @description
    #' Obtain the names of the features.
    #' @return A `list` of `character` vectors.
    feature_names = function() {
      stats::setNames(
        lapply(
          self$problems,
          function(x) x$feature_names()
        ),
        self$problem_names()
      )
    },

    #' @description
    #' Obtain the names of the actions.
    #' @return A `character` vector.
    action_names = function() {
      self$problems[[1]]$action_names()
    },

    #' @description
    #' Obtain the names of the projects.
    #' @return A `list` of `character` vectors.
    project_names = function() {
      stats::setNames(
        lapply(
          self$problems,
          function(x) x$project_names()
        ),
        self$problem_names()
      )
    },

    #' @description
    #' Create a new object with an approach added to the problem formulation.
    #' @param x [MultiObjApproach-class] object.
    #' @return An updated `MultiObjProjectProblem` object.
    add_approach = function(x) {
      assertthat::assert_that(inherits(x, "MultiObjApproach"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$approach)) {
        warning(
          "Overwriting previously defined approach.",
          call. = FALSE, immediate. = TRUE
        )
      } else {
        p$defaults$approach <- FALSE
      }
      p$approach <- x
      p
    },

    #' @description
    #' Create a new object with a solver added to the problem formulation.
    #' @param x [Solver-class] object.
    #' @return An updated `MultiObjProjectProblem` object.
    add_solver = function(x) {
      assertthat::assert_that(inherits(x, "Solver"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$solver)) {
        warning(
          "Overwriting previously defined solver.",
          call. = FALSE, immediate. = TRUE
        )
      } else {
        p$defaults$solver <- FALSE
      }
      p$solver <- x
      p
    }
  )
)
