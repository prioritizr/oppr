#' @include internal.R waiver.R pproto.R Collection-proto.R
NULL

#' @export
if (!methods::isClass("ProjectProblem")) methods::setOldClass("ProjectProblem")
NULL

#' Project problem class
#'
#' @section Description:
#' This class is used to represent project prioritization problems. A
#' project prioritization problem has actions, projects,
#' and features. Features are the biological entities that need to
#' be conserved (e.g. species, populations, ecosystems). Actions are
#' real-world management actions that can be implemented for conservation
#' purposes (e.g. habitat restoration, monitoring, pest eradication). Each
#' action should have a known cost, and this usually means that each
#' action should have a defined spatial extent and time period (though this
#' is not necessary). Conservation projects are groups of management actions
#' (they can also comprise a singular action too), and each project is
#' associated with a probability of success if all of its associated actions
#' are funded. To determine which projects should be funded, each project is
#' associated with an probability of persistence for the
#' features that they benefit. These values should indicate the
#' probability that each feature will persist if only that project funded
#' and not the additional benefit relative to the baseline project. Missing
#' (\code{NA}) values should be used to indicate which projects do not
#' enhance the probability of certain features.
#'
#' Given these data, a project prioritization problem involves making a
#' decision about which actions should be funded or not---and in turn, which
#' projects should be funded or not---to maximize or minimize a specific
#' objective whilst meeting specific constraints. The objective for a project
#' prioritization problem will \emph{always} pertain to the probability that
#' features are expected to persist. For example, an objective for a project
#' prioritization problem could be to maximize the maximize the total amount of
#' species that are expected to persist, or minimize the total cost of the
#' funded actions subject to constraints which ensure that each feature meets a
#' target level of persistence. The constraints in a project prioritization
#' problem can be used to specify additional requirements (e.g. certain
#' actions must be funded). Finally, a project prioritization problem---unlike
#' an optimization problem---also requires a method to solve the problem.
#' \strong{This class represents a planning problem, to actually build and then
#' solve a planning problem, use the \code{\link{problem}} function. Only
#' experts should use this class directly.}
#'
#' @section Fields:
#' \describe{
#'
#' \item{$data}{\code{list} object containing data.}
#'
#' \item{$objective}{\code{\link{Objective-class}} object used to represent how
#'   the targets relate to the solution.}
#'
#' \item{$decisions}{\code{\link{Decision-class}} object used to represent the
#'   type of decision made on planning units.}
#'
#' \item{$targets}{\code{\link{Target-class}} object used to represent
#'   representation targets for features.}
#'
#' \item{$weights}{\code{\link{Weight-class}} object used to represent
#'   feature weights.}
#'
#' \item{$constraints}{\code{\link{Collection-class}} object used to represent
#'   additional \code{\link{constraints}} that the problem is subject to.}
#'
#' \item{$solver}{\code{\link{Solver-class}} object used to solve the problem.}
#'
#' }
#'
#' @section Usage:
#'
#' \code{x$print()}
#'
#' \code{x$show()}
#'
#' \code{x$repr()}
#'
#' \code{x$get_data(name)}
#'
#' \code{x$set_data(name, value)}
#'
#' \code{number_of_actions()}
#'
#' \code{number_of_projects()}
#'
#' \code{number_of_features()}
#'
#' \code{action_names()}
#'
#' \code{project_names()}
#'
#' \code{feature_names()}
#'
#' \code{feature_weights()}
#'
#' \code{feature_phylogeny()}
#'
#' \code{action_costs()}
#'
#' \code{project_costs()}
#'
#' \code{project_success_probabilities()}
#'
#' \code{pf_matrix()}
#'
#' \code{epf_matrix()}
#'
#' \code{pa_matrix()}
#'
#' \code{x$add_objective(obj)}
#'
#' \code{x$add_decisions(dec)}
#'
#' \code{x$add_constraint(con)}
#'
#' \code{x$add_solver(sol)}
#'
#' \code{x$add_targets(targ)}
#'
#' \code{x$add_weights(wt)}
#'
#' \code{x$get_constraint_parameter(id)}
#'
#' \code{x$set_constraint_parameter(id, value)}
#'
#' \code{x$render_constraint_parameter(id)}
#'
#' \code{x$render_all_constraint_parameters()}
#'
#' \code{x$get_objective_parameter(id)}
#'
#' \code{x$set_objective_parameter(id, value)}
#'
#' \code{x$render_objective_parameter(id)}
#'
#' \code{x$render_all_objective_parameters()}
#'
#' \code{x$get_solver_parameter(id)}
#'
#' \code{x$set_solver_parameter(id, value)}
#'
#' \code{x$render_solver_parameter(id)}
#'
#' \code{x$render_all_solver_parameters()}
#'
#' @section Arguments:
#'
#' \describe{
#'
#' \item{name}{\code{character} name for object.}
#'
#' \item{value}{an object.}
#'
#' \item{obj}{\code{\link{Objective-class}} object.}
#'
#' \item{wt}{\code{\link{Weight-class}} object.}
#'
#' \item{dec}{\code{\link{Decision-class}} object.}
#'
#' \item{con}{\code{\link{Constraint-class}} object.}
#'
#' \item{sol}{\code{\link{Solver-class}} object.}
#'
#' \item{targ}{\code{\link{Target-class}} object.}
#'
#' \item{wt}{\code{\link{Weight-class}} object.}
#'
#' \item{id}{\code{Id} object that refers to a specific parameter.}
#'
#' \item{value}{object that the parameter value should become.}
#'
#' }
#'
#' @section Details:
#' \describe{
#'
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{return \code{character} representation of the object.}
#'
#' \item{get_data}{return an object stored in the \code{data} field with
#'   the corresponding \code{name}. If the object is not present in the
#'   \code{data} field, a \code{waiver} object is returned.}
#'
#' \item{set_data}{store an object stored in the \code{data} field with
#'   the corresponding name. If an object with that name already
#'   exists then the object is overwritten.}
#'
#' \item{number_of_actions}{\code{integer} number of actions.}
#'
#' \item{number_of_projects}{\code{integer} number of projects.}
#'
#' \item{number_of_features}{\code{integer} number of features.}
#'
#' \item{action_names}{\code{character} names of actions in the problem.}
#'
#' \item{project_names}{\code{character} names of projects in the problem.}
#'
#' \item{feature_names}{\code{character} names of features in the problem.}
#'
#' \item{feature_weights}{\code{character} feature weights.}
#'
#' \item{feature_phylogeny}{\code{\link[ape]{phylo}} phylogenetic tree object.}
#'
#' \item{action_costs}{\code{numeric} costs for each action.}
#'
#' \item{project_costs}{\code{numeric} costs for each project.}
#'
#' \item{project_success_probabilities}{\code{numeric} probability that
#'   each project will succeed.}
#'
#' \item{pf_matrix}{
#'   \code{\link[Matrix]{dgCMatrix-class}} object denoting the enhanced
#'   probability that features will persist if different projects are funded.}
#'
#' \item{epf_matrix}{
#'   \code{\link[Matrix]{dgCMatrix-class}} object denoting the enhanced
#'   probability that features is expected to persist if different projects are
#'  funded. This is calculated as the \code{pf_matrix} multiplied by the
#'  project success probabilities.}
#'
#' \item{pa_matrix}{
#'   \code{\link[Matrix]{dgCMatrix-class}} object indicating which actions are
#'   associated with which projects.}
#'
#' \item{feature_targets}{\code{\link[tibble]{tibble}} with feature targets.}
#'
#' \item{add_objective}{return a new  \code{\link{ProjectProblem-class}}
#'   with the objective added to it.}
#'
#' \item{add_decisions}{return a new \code{\link{ProjectProblem-class}}
#'   object with the decision added to it.}
#'
#' \item{add_solver}{return a new \code{\link{ProjectProblem-class}} object
#'   with the solver added to it.}
#'
#' \item{add_constraint}{return a new \code{\link{ProjectProblem-class}}
#'   object with the constraint added to it.}
#'
#' \item{add_targets}{return a copy with the targets added to the problem.}
#'
#' \item{get_constraint_parameter}{get the value of a parameter (specified by
#'   argument \code{id}) used in one of the constraints in the object.}
#'
#' \item{set_constraint_parameter}{set the value of a parameter (specified by
#'   argument \code{id}) used in one of the constraints in the object to
#'   \code{value}.}
#'
#' \item{render_constraint_parameter}{generate a \emph{shiny} widget to modify
#'  the value of a parameter (specified by argument \code{id}).}
#'
#' \item{render_all_constraint_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters' widgets.}
#'
#' \item{get_objective_parameter}{get the value of a parameter (specified by
#'   argument \code{id}) used in the object's objective.}
#'
#' \item{set_objective_parameter}{set the value of a parameter (specified by
#'   argument \code{id}) used in the object's objective to \code{value}.}
#'
#' \item{render_objective_parameter}{generate a \emph{shiny} widget to modify
#'   the value of a parameter (specified by argument \code{id}).}
#'
#' \item{render_all_objective_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters' widgets.}
#'
#' \item{get_weight_parameter}{get the value of a parameter (specified by
#'   argument \code{id}) used in the object's weights.}
#'
#' \item{set_weight_parameter}{set the value of a parameter (specified by
#'   argument \code{id}) used in the object's weights to \code{value}.}
#'
#' \item{render_weight_parameter}{generate a \emph{shiny} widget to modify
#'   the value of a parameter (specified by argument \code{id}).}
#'
#' \item{render_all_weight_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters' widgets.}
#'
#' \item{get_solver_parameter}{get the value of a parameter (specified by
#'   argument \code{id}) used in the object's solver.}
#'
#' \item{set_solver_parameter}{set the value of a parameter (specified by
#'   argument \code{id}) used in the object's solver to \code{value}.}
#'
#' \item{render_solver_parameter}{generate a \emph{shiny} widget to modify
#'   the value of a parameter (specified by argument \code{id}).}
#'
#' \item{render_all_solver_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters' widgets.}
#'
#' }
#'
#' @name ProjectProblem-class
#'
#' @aliases ProjectProblem
NULL

#' @export
ProjectProblem <- pproto(
  "ProjectProblem",
  data = list(),
  objective = new_waiver(),
  weights = new_waiver(),
  decisions = new_waiver(),
  targets = new_waiver(),
  constraints = pproto(NULL, Collection),
  solver = new_waiver(),
  print = function(self) {
    r <- vapply(list(self$objective, self$targets), function(x) {
      if (is.Waiver(x))
        return("none")
      return(x$repr())
    }, character(1))
    d <- vapply(list(self$decisions, self$solver), function(x) {
      if (is.Waiver(x))
        return("default")
      return(x$repr())
    }, character(1))
    if (is.Waiver(self$weights)) {
      w <- "default"
    } else {
      w <- round(self$feature_weights(), 5)
      w <- paste0("min: ", min(w), ", max: ", max(w))
    }
    pr <- round(range(self$project_success_probabilities(), na.rm = TRUE), 5)
    cs <- round(range(self$action_costs(), na.rm = TRUE), 5)
    message(paste0("Project Prioritization Problem",
    "\n  actions          ", repr_atomic(self$action_names(), "actions"),
    "\n  projects         ", repr_atomic(self$project_names(), "projects"),
    "\n  features         ", repr_atomic(self$feature_names(), "features"),
    "\n  action costs:    min: ", cs[1], ", max: ", cs[2],
    "\n  project success: min: ", pr[1], ", max: ", pr[2],
    "\n  objective:       ", r[1],
    "\n  targets:         ", r[2],
    "\n  weights:         ", w,
    "\n  decisions        ", d[1],
    "\n  constraints:     ", align_text(self$constraints$repr(), 20),
    "\n  solver:          ", d[2]))
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    "ProjectProblem object"
  },
  get_data = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x))
    if (!x %in% names(self$data))
      return(new_waiver())
    return(self$data[[x]])
  },
  set_data = function(self, x, value) {
    assertthat::assert_that(assertthat::is.string(x))
    self$data[[x]] <- value
    invisible()
  },
  number_of_actions = function(self) {
    nrow(self$data$actions)
  },
  number_of_projects = function(self) {
    nrow(self$data$projects)
  },
  number_of_features = function(self) {
    nrow(self$data$features)
  },
  action_names = function(self) {
    as.character(self$data$actions[[self$data$action_name_column]])
  },
  project_names = function(self) {
    as.character(self$data$projects[[self$data$project_name_column]])
  },
  feature_names = function(self) {
    as.character(self$data$features[[self$data$feature_name_column]])
  },
  feature_weights = function(self) {
    if (is.Waiver(self$weights))
      return(self$objective$default_feature_weights())
    self$weights$output()
  },
  feature_phylogeny = function(self) {
    if (is.Waiver(self$objective))
      stop("problem is missing objective")
    self$objective$feature_phylogeny()
  },
  action_costs = function(self) {
    setNames(self$data$actions[[self$data$action_cost_column]],
             self$action_names())
  },
  project_costs = function(self) {
    pa <- as.matrix(self$pa_matrix())
    ac <- matrix(self$action_costs(), ncol = ncol(pa), nrow = nrow(pa),
                 byrow = TRUE)
    rowSums(pa * ac)
  },
  project_success_probabilities = function(self) {
    setNames(self$data$projects[[self$data$project_success_column]],
             self$project_names())
  },
  pf_matrix = function(self) {
    m <- methods::as(as.matrix(
      self$data$projects[, self$data$features[[self$data$feature_name_column]],
                         drop = FALSE]), "dgCMatrix")
    m@x[is.na(m@x)] <- 0
    rownames(m) <- self$project_names()
    colnames(m) <- self$feature_names()
    Matrix::drop0(m)
  },
  epf_matrix = function(self) {
    # extract persistence probabilities, but not accounting for baseline
    m <- as(self$pf_matrix() * matrix(self$project_success_probabilities(),
                                      ncol = self$number_of_features(),
                                      nrow = self$number_of_projects()),
                                      "dgCMatrix")
    m <- as(m, "dgCMatrix")
    m <- Matrix::drop0(m)
    # if include baseline probabilities, then account for probabilities of
    # each project persisting and the baseline project not failing
    if (self$data$adjust_for_baseline) {
      ## extract project costs
      pc <- self$project_costs()
      ## extract baseline probability data
      bp <- which(pc == 0)
      bpp <- m[bp, ]
      ## update probabilities
      m2 <- m + ((1 - m) * m[rep(bp, nrow(m)), ])
      m <- m2 * (m > 0)
      ## overwrite baseline data
      m[bp, ] <- bpp
      ## coerce data type
      m <- as(m, "dgCMatrix")
      m <- Matrix::drop0(m)
    }
    rownames(m) <- self$project_names()
    colnames(m) <- self$feature_names()
    m
  },
  pa_matrix = function(self) {
    m <- methods::as(as.matrix(
      self$data$projects[, self$data$actions[[self$data$action_name_column]],
                         drop = FALSE]), "dgCMatrix")
    rownames(m) <- self$data$projects[[self$data$project_name_column]]
    m
  },
  feature_targets = function(self) {
    if (is.Waiver(self$targets))
      stop("problem is missing targets")
    self$targets$output()
  },
  add_solver = function(self, x) {
    assertthat::assert_that(inherits(x, "Solver"))
    if (!is.Waiver(self$solver))
      warning("overwriting previously defined solver")
    pproto(NULL, self, solver = x)
  },
  add_targets = function(self, x) {
    assertthat::assert_that(inherits(x, "Target"))
    if (!is.Waiver(self$targets))
      warning("overwriting previously defined targets")
    pproto(NULL, self, targets = x)
  },
  add_weights = function(self, x) {
    assertthat::assert_that(inherits(x, "Weight"))
    if (!is.Waiver(self$weights))
      warning("overwriting previously defined weights")
    pproto(NULL, self, weights = x)
  },
  add_objective = function(self, x) {
    assertthat::assert_that(inherits(x, "Objective"))
    if (!is.Waiver(self$objective))
      warning("overwriting previously defined objective")
    pproto(NULL, self, objective = x)
  },
  add_decisions = function(self, x) {
    assertthat::assert_that(inherits(x, "Decision"))
    if (!is.Waiver(self$decisions))
      warning("overwriting previously defined decision")
    pproto(NULL, self, decisions = x)
  },
  add_constraint = function(self, x) {
    assertthat::assert_that(inherits(x, "Constraint"))
    p <- pproto(NULL, self)
    p$constraints$add(x)
    return(p)
  },
  get_constraint_parameter = function(self, id) {
    self$constraints$get_parameter(id)
  },
  set_constraint_parameter = function(self, id, value) {
    self$constraints$set_parameter(id, value)
  },
  render_constraint_parameter = function(self, id) {
    self$constraints$render_parameter(id)
  },
  render_all_constraint_parameters = function(self) {
    self$constraints$render_all_parameter()
  },
  get_objective_parameter = function(self, id) {
    self$objective$get_parameter(id)
  },
  set_objective_parameter = function(self, id, value) {
    self$objective$set_parameter(id, value)
  },
  render_objective_parameter = function(self, id) {
    self$objective$render_parameter(id)
  },
  render_all_objective_parameters = function(self) {
    self$objective$render_all_parameter()
  },
  get_solver_parameter = function(self, id) {
    self$solver$get_parameter(id)
  },
  set_solver_parameter = function(self, id, value) {
    self$solver$set_parameter(id, value)
  },
  render_solver_parameter = function(self, id) {
    self$solver$render_parameter(id)
  },
  render_all_solver_parameters = function(self) {
    self$solver$render_all_parameter()
  })
