#' @include internal.R waiver.R
NULL

#' @export
if (!methods::isClass("ProjectProblem")) methods::setOldClass("ProjectProblem")
NULL

#' Project problem class
#'
#' @section Description:
#' This class is used to represent project prioritization problems.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name ProjectProblem-class
#'
#' @family classes
#'
#' @aliases ProjectProblem
#'
#' @export
ProjectProblem <- R6::R6Class(
  "ProjectProblem",
  public = list(
    #' @field data `list` containing data (e.g., projects, features).
    data = list(),

    #' @field defaults `list` indicating if other fields contain defaults.
    defaults = list(
      objective = TRUE,
      decisions = TRUE,
      targets = TRUE,
      weights = TRUE,
      constraints = TRUE,
      solver = TRUE
    ),

    #' @field objective [Objective-class] object used to specify the objective
    #' function.
    objective = new_waiver(),

    #' @field weights [Weight-class] object used to specify the objective
    #' function.
    weights = new_waiver(),

    #' @field decisions [Decision-class] object used to represent the type of
    #' decision made on planning units.
    decisions = new_waiver(),

    #' @field targets [Target-class] object used to represent representation
    #' targets for features.
    targets = new_waiver(),

    #' @field constraints `list` object used to store [Constraint-class]
    #' objects for the problem.
    constraints = list(),

    #' @field solver [Solver-class] object used to specify the process for
    #' generating a solution.
    solver = new_waiver(),

    #' @description
    #' Create a new conservation problem object.
    #' @param data `list` containing data
    #' @return A new `ProjectProblem` object.
    initialize = function(data = list()) {
      self$data <- data
    },

    #' @description
    #' Print concise information about the object.
    #' @return Invisible `TRUE`.
    print = function() {
      # specify text to display
      obj_msg <- "none specified"
      if (!isTRUE(self$defaults$objective)) {
        obj_msg <- self$objective$repr()
      }
      targets_msg <- "none specified"
      if (!isTRUE(self$defaults$targets)) {
        targets_msg <- self$targets$repr()
      }
      weights_msg <- "none specified"
      if (!isTRUE(self$defaults$weights)) {
        weights_msg <- self$weights$repr()
      }
      decisions_msg <- "none specified"
      if (!isTRUE(self$defaults$decisions)) {
        decisions_msg <- self$decisions$repr()
      }
      constraints_msg <- "none specified"
      if (length(self$constraints) > 0) {
        constraints_msg <- paste(
          vapply(self$constraints, function(x) x$repr(), character(1)),
          collapse = ", "
        )
      }
      solver_msg <- "none specified"
      if (!isTRUE(self$defaults$solver)) {
        solver_msg <- self$solver$repr()
      }
      # display message
      message(
        paste0(
          "Project Prioritization Problem",
          "\nactions:         ",
          repr_options(self$action_names(), "actions"),
          "\nprojects:        ",
          repr_options(self$project_names(), "projects"),
          "\nfeatures:        ",
          repr_options(self$feature_names(), "features"),
          "\naction costs:    ",
          repr_values(self$action_costs()),
          "\nproject success: ",
          repr_values(self$project_success_probabilities()),
          "\nobjective:       ", obj_msg,
          "\ntargets:         ", targets_msg,
          "\nweights:         ", weights_msg,
          "\nconstraints:     ", constraints_msg,
          "\ndecisions:       ", decisions_msg,
          "\nsolver:          ", solver_msg
        )
      )
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
      "ProjectProblem object"
    },

    #' @description
    #' Get values stored in the `data` field.
    #' @param x `character` name of data.
    #' @return An object. If the `data` field does not contain an object
    #' associated with the argument to `x`, then a [new_waiver()] object is
    #' returned.
    get_data = function(x) {
      assertthat::assert_that(assertthat::is.string(x))
      if (!x %in% names(self$data)) {
        return(new_waiver())
      }
      return(self$data[[x]])
    },

    #' @description
    #' Set values stored in the `data` field. Note that this method will
    #' overwrite existing data.
    #' @param x `character` name of data.
    #' @param value Object to store.
    #' @return Invisible `TRUE`.
    set_data = function(x, value) {
      assertthat::assert_that(assertthat::is.string(x))
      self$data[[x]] <- value
      invisible()
    },

    #' @description
    #' Obtain the number of actions.
    #' @return An `integer` value.
    number_of_actions = function() {
      nrow(self$data$actions)
    },

    #' @description
    #' Obtain the number of projects.
    #' @return An `integer` value.
    number_of_projects = function() {
      nrow(self$data$projects)
    },

    #' @description
    #' Obtain the number of features.
    #' @return An `integer` value.
    number_of_features = function() {
      nrow(self$data$features)
    },

    #' @description
    #' Obtain the names of the actions.
    #' @return A `character` vector.
    action_names = function() {
      as.character(self$data$actions[[self$data$action_name_column]])
    },

    #' @description
    #' Obtain the names of the projects.
    #' @return A `character` vector.
    project_names = function() {
      as.character(self$data$projects[[self$data$project_name_column]])
    },

    #' @description
    #' Obtain the names of the features.
    #' @return A `character` vector.
    feature_names = function() {
      as.character(self$data$features[[self$data$feature_name_column]])
    },

    #' @description
    #' Obtain the feature weights.
    #' @return A named `numeric` vector.
    feature_weights = function() {
      if (is.Waiver(self$weights)) {
        return(self$objective$default_feature_weights())
      }
      c(self$weights$output())
    },

    #' @description
    #' Obtain the feature targets.
    #' @return A [tibble::tibble()] object.
    feature_targets = function() {
      if (is.Waiver(self$targets)) {
        stop("problem is missing targets", call. = FALSE)
      }
      self$targets$output()
    },

    #' @description
    #' Obtain the feature phylogeny.
    #' @return A [ape::phylo()] phylogenetic tree object.
    feature_phylogeny = function() {
      if (is.Waiver(self$objective)) {
        stop("problem is missing objective", call. = FALSE)
      }
      self$objective$feature_phylogeny()
    },

    #' @description
    #' Obtain the action costs.
    #' @return A `numeric` vector.
    action_costs = function() {
      setNames(
        self$data$actions[[self$data$action_cost_column]],
        self$action_names()
      )
    },

    #' @description
    #' Obtain the project costs.
    #' @return A `numeric` vector.
    project_costs = function() {
      pa <- as.matrix(self$pa_matrix())
      ac <- matrix(
        self$action_costs(),
        ncol = ncol(pa), nrow = nrow(pa), byrow = TRUE
      )
      rowSums(pa * ac)
    },

    #' @description
    #' Obtain the probability that each project will succeed if funded.
    #' @return A `numeric` vector.
    project_success_probabilities = function() {
      setNames(
        self$data$projects[[self$data$project_success_column]],
        self$project_names()
      )
    },

    #' @description
    #' Obtain information on the outcome for each feature that would be
    #' expected if each project funded and is successfully completed.
    #' @return A [Matrix::dgCMatrix-class] object.
    of_matrix = function() {
      m <- as_Matrix(
        as.matrix(
          self$data$projects[,
            self$data$features[[self$data$feature_name_column]],
            drop = FALSE
          ]
        ),
        "dgCMatrix"
      )
      m@x[is.na(m@x)] <- 0
      rownames(m) <- self$project_names()
      colnames(m) <- self$feature_names()
      Matrix::drop0(m)
    },

    #' @description
    #' Obtain information on which actions are associated with each project.
    #' @return A [Matrix::dgCMatrix-class] object.
    pa_matrix = function() {
      m <- as_Matrix(
        as.matrix(
          self$data$projects[,
            self$data$actions[[self$data$action_name_column]],
            drop = FALSE
          ]
        ),
        "dgCMatrix"
      )
      rownames(m) <- self$data$projects[[self$data$project_name_column]]
      m
    },

    #' @description
    #' Calculate the expected outcome for each feature assuming that each
    #' project is funded and accounting for the possibility that
    #' funded projects may fail to be successfully completed.
    #' @return A [Matrix::dgCMatrix-class] object.
    eof_matrix = function() {
      # extract the project outcome data and multiply by success probabilities
      m <- as_Matrix(
        self$of_matrix() *
          matrix(
            self$project_success_probabilities(),
            ncol = self$number_of_features(),
            nrow = self$number_of_projects()
          ),
        "dgCMatrix"
      )
      m <- Matrix::drop0(m)
      # if include baseline probabilities, then account for probabilities of
      # each project persisting and the baseline project not failing
      if (isTRUE(self$data$adjust_for_baseline)) {
        ## extract project costs
        pc <- self$project_costs()
        ## extract baseline probability data
        bp <- which(pc == 0)
        assertthat::assert_that(
          identical(length(bp), 1L),
          msg = paste(
            "Can only adjust for baseline if there is a",
            "ingle zero-cost project."
          )
        )
        bpp <- m[bp, ]
        ## update probabilities
        m2 <- m + ((1 - m) * m[rep(bp, nrow(m)), ])
        m <- m2 * (m > 0)
        ## overwrite baseline data
        m[bp, ] <- bpp
        ## coerce data type
        m <- as_Matrix(m, "dgCMatrix")
        m <- Matrix::drop0(m)
      }
      # set attribute names
      rownames(m) <- self$project_names()
      colnames(m) <- self$feature_names()
      # return result
      m
    },

    #' @description
    #' Create a new object with a solver added to the problem formulation.
    #' @param x [Solver-class] object.
    #' @return An updated `ProjectProblem` object.
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
    },

    #' @description
    #' Create a new object with targets added to the problem formulation.
    #' @param x [Target-class] object.
    #' @return An updated `ProjectProblem` object.
    add_targets = function(x) {
      assertthat::assert_that(inherits(x, "Target"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$targets)) {
        warning(
          "Overwriting previously defined targets.",
          call. = FALSE, immediate. = TRUE
        )
      } else {
        p$defaults$targets <- FALSE
      }
      p$targets <- x
      p
    },

    #' @description
    #' Create a new object with weights added to the problem formulation.
    #' @param x [Weight-class] object.
    #' @return An updated `ProjectProblem` object.
    add_weights = function(x) {
      assertthat::assert_that(inherits(x, "Weight"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$weights)) {
        warning(
          "Overwriting previously defined weights.",
          call. = FALSE, immediate. = TRUE
        )
      } else {
        p$defaults$weights <- FALSE
      }
      p$weights <- x
      p
    },

    #' @description
    #' Create a new object with the objective added to the problem formulation.
    #' @param x [Objective-class] object.
    #' @return An updated `ProjectProblem` object.
    add_objective = function(x) {
      assertthat::assert_that(inherits(x, "Objective"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$objective)) {
        warning(
          "Overwriting previously defined objective.",
          call. = FALSE, immediate. = TRUE
        )
      } else {
        p$defaults$objective <- FALSE
      }
      p$objective <- x
      p
    },

    #' @description
    #' Create a new object with the decisions added to the problem formulation.
    #' @param x [Objective-class] object.
    #' @return An updated `ProjectProblem` object.
    add_decisions = function(x) {
      assertthat::assert_that(inherits(x, "Decision"))
      p <- self$clone(deep = TRUE)
      if (!isTRUE(p$defaults$decisions)) {
        warning(
          "Overwriting previously defined decisions.",
          call. = FALSE, immediate. = TRUE
        )
      } else {
        p$defaults$decisions <- FALSE
      }
      p$decisions <- x
      p
    },

    #' @description
    #' Create a new object with the constraint added to the problem formulation.
    #' @param x [Constraint-class] object.
    #' @return An updated `ProjectProblem` object.
    add_constraint = function(x) {
      assertthat::assert_that(inherits(x, "Constraint"))
      p <- self$clone(deep = TRUE)
      p$constraints[[length(p$constraints) + 1]] <- x
      p
    }
  )
)
