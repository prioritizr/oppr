# Project problem class

Project problem class

## Description

This class is used to represent project prioritization problems. **Only
experts should use the fields and methods for this class directly.**

## See also

Other classes:
[`Constraint-class`](https://prioritizr.github.io/oppr/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.github.io/oppr/reference/Decision-class.md),
[`MultiObjApproach-class`](https://prioritizr.github.io/oppr/reference/MultiObjApproach-class.md),
[`MultiObjProjectProblem-class`](https://prioritizr.github.io/oppr/reference/MultiObjProjectProblem-class.md),
[`Objective-class`](https://prioritizr.github.io/oppr/reference/Objective-class.md),
[`OptimizationProblem-class`](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md),
[`ProjectModifier-class`](https://prioritizr.github.io/oppr/reference/ProjectModifier-class.md),
[`Solver-class`](https://prioritizr.github.io/oppr/reference/Solver-class.md),
[`Target-class`](https://prioritizr.github.io/oppr/reference/Target-class.md),
[`Weight-class`](https://prioritizr.github.io/oppr/reference/Weight-class.md)

## Public fields

- `data`:

  `list` containing data (e.g., projects, features).

- `defaults`:

  `list` indicating if other fields contain defaults.

- `objective`:

  [Objective](https://prioritizr.github.io/oppr/reference/Objective-class.md)
  object used to specify the objective function.

- `weights`:

  [Weight](https://prioritizr.github.io/oppr/reference/Weight-class.md)
  object used to specify the objective function.

- `decisions`:

  [Decision](https://prioritizr.github.io/oppr/reference/Decision-class.md)
  object used to represent the type of decision made on planning units.

- `targets`:

  [Target](https://prioritizr.github.io/oppr/reference/Target-class.md)
  object used to represent representation targets for features.

- `constraints`:

  `list` object used to store
  [Constraint](https://prioritizr.github.io/oppr/reference/Constraint-class.md)
  objects for the problem.

- `solver`:

  [Solver](https://prioritizr.github.io/oppr/reference/Solver-class.md)
  object used to specify the process for generating a solution.

## Methods

### Public methods

- [`ProjectProblem$new()`](#method-ProjectProblem-initialize)

- [`ProjectProblem$print()`](#method-ProjectProblem-print)

- [`ProjectProblem$show()`](#method-ProjectProblem-show)

- [`ProjectProblem$repr()`](#method-ProjectProblem-repr)

- [`ProjectProblem$get_data()`](#method-ProjectProblem-get_data)

- [`ProjectProblem$set_data()`](#method-ProjectProblem-set_data)

- [`ProjectProblem$number_of_actions()`](#method-ProjectProblem-number_of_actions)

- [`ProjectProblem$number_of_projects()`](#method-ProjectProblem-number_of_projects)

- [`ProjectProblem$number_of_features()`](#method-ProjectProblem-number_of_features)

- [`ProjectProblem$action_names()`](#method-ProjectProblem-action_names)

- [`ProjectProblem$project_names()`](#method-ProjectProblem-project_names)

- [`ProjectProblem$feature_names()`](#method-ProjectProblem-feature_names)

- [`ProjectProblem$feature_weights()`](#method-ProjectProblem-feature_weights)

- [`ProjectProblem$feature_targets()`](#method-ProjectProblem-feature_targets)

- [`ProjectProblem$feature_phylogeny()`](#method-ProjectProblem-feature_phylogeny)

- [`ProjectProblem$action_costs()`](#method-ProjectProblem-action_costs)

- [`ProjectProblem$project_costs()`](#method-ProjectProblem-project_costs)

- [`ProjectProblem$project_success_probabilities()`](#method-ProjectProblem-project_success_probabilities)

- [`ProjectProblem$of_matrix()`](#method-ProjectProblem-of_matrix)

- [`ProjectProblem$pa_matrix()`](#method-ProjectProblem-pa_matrix)

- [`ProjectProblem$eof_matrix()`](#method-ProjectProblem-eof_matrix)

- [`ProjectProblem$add_solver()`](#method-ProjectProblem-add_solver)

- [`ProjectProblem$add_targets()`](#method-ProjectProblem-add_targets)

- [`ProjectProblem$add_weights()`](#method-ProjectProblem-add_weights)

- [`ProjectProblem$add_objective()`](#method-ProjectProblem-add_objective)

- [`ProjectProblem$add_decisions()`](#method-ProjectProblem-add_decisions)

- [`ProjectProblem$add_constraint()`](#method-ProjectProblem-add_constraint)

- [`ProjectProblem$clone()`](#method-ProjectProblem-clone)

------------------------------------------------------------------------

### `ProjectProblem$new()`

Create a new conservation problem object.

#### Usage

    ProjectProblem$new(data = list())

#### Arguments

- `data`:

  `list` containing data

#### Returns

A new `ProjectProblem` object.

------------------------------------------------------------------------

### `ProjectProblem$print()`

Print concise information about the object.

#### Usage

    ProjectProblem$print()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `ProjectProblem$show()`

Display concise information about the object.

#### Usage

    ProjectProblem$show()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `ProjectProblem$repr()`

Generate a character representation of the object.

#### Usage

    ProjectProblem$repr()

#### Returns

A `character` value.

------------------------------------------------------------------------

### `ProjectProblem$get_data()`

Get values stored in the `data` field.

#### Usage

    ProjectProblem$get_data(x)

#### Arguments

- `x`:

  `character` name of data.

#### Returns

An object. If the `data` field does not contain an object associated
with the argument to `x`, then a
[`new_waiver()`](https://prioritizr.github.io/oppr/reference/new_waiver.md)
object is returned.

------------------------------------------------------------------------

### `ProjectProblem$set_data()`

Set values stored in the `data` field. Note that this method will
overwrite existing data.

#### Usage

    ProjectProblem$set_data(x, value)

#### Arguments

- `x`:

  `character` name of data.

- `value`:

  Object to store.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `ProjectProblem$number_of_actions()`

Obtain the number of actions.

#### Usage

    ProjectProblem$number_of_actions()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `ProjectProblem$number_of_projects()`

Obtain the number of projects.

#### Usage

    ProjectProblem$number_of_projects()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `ProjectProblem$number_of_features()`

Obtain the number of features.

#### Usage

    ProjectProblem$number_of_features()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `ProjectProblem$action_names()`

Obtain the names of the actions.

#### Usage

    ProjectProblem$action_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### `ProjectProblem$project_names()`

Obtain the names of the projects.

#### Usage

    ProjectProblem$project_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### `ProjectProblem$feature_names()`

Obtain the names of the features.

#### Usage

    ProjectProblem$feature_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### `ProjectProblem$feature_weights()`

Obtain the feature weights.

#### Usage

    ProjectProblem$feature_weights()

#### Returns

A named `numeric` vector.

------------------------------------------------------------------------

### `ProjectProblem$feature_targets()`

Obtain the feature targets.

#### Usage

    ProjectProblem$feature_targets()

#### Returns

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### `ProjectProblem$feature_phylogeny()`

Obtain the feature phylogeny.

#### Usage

    ProjectProblem$feature_phylogeny()

#### Returns

A [`ape::phylo()`](https://rdrr.io/pkg/ape/man/read.tree.html)
phylogenetic tree object.

------------------------------------------------------------------------

### `ProjectProblem$action_costs()`

Obtain the action costs.

#### Usage

    ProjectProblem$action_costs()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### `ProjectProblem$project_costs()`

Obtain the project costs.

#### Usage

    ProjectProblem$project_costs()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### `ProjectProblem$project_success_probabilities()`

Obtain the probability that each project will succeed if funded.

#### Usage

    ProjectProblem$project_success_probabilities()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### `ProjectProblem$of_matrix()`

Obtain information on the outcome for each feature that would be
expected if each project funded and is successfully completed.

#### Usage

    ProjectProblem$of_matrix()

#### Returns

A
[Matrix::dgCMatrix](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
object.

------------------------------------------------------------------------

### `ProjectProblem$pa_matrix()`

Obtain information on which actions are associated with each project.

#### Usage

    ProjectProblem$pa_matrix()

#### Returns

A
[Matrix::dgCMatrix](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
object.

------------------------------------------------------------------------

### `ProjectProblem$eof_matrix()`

Calculate the expected outcome for each feature assuming that each
project is funded and accounting for the possibility that funded
projects may fail to be successfully completed.

#### Usage

    ProjectProblem$eof_matrix()

#### Returns

A
[Matrix::dgCMatrix](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
object.

------------------------------------------------------------------------

### `ProjectProblem$add_solver()`

Create a new object with a solver added to the problem formulation.

#### Usage

    ProjectProblem$add_solver(x)

#### Arguments

- `x`:

  [Solver](https://prioritizr.github.io/oppr/reference/Solver-class.md)
  object.

#### Returns

An updated `ProjectProblem` object.

------------------------------------------------------------------------

### `ProjectProblem$add_targets()`

Create a new object with targets added to the problem formulation.

#### Usage

    ProjectProblem$add_targets(x)

#### Arguments

- `x`:

  [Target](https://prioritizr.github.io/oppr/reference/Target-class.md)
  object.

#### Returns

An updated `ProjectProblem` object.

------------------------------------------------------------------------

### `ProjectProblem$add_weights()`

Create a new object with weights added to the problem formulation.

#### Usage

    ProjectProblem$add_weights(x)

#### Arguments

- `x`:

  [Weight](https://prioritizr.github.io/oppr/reference/Weight-class.md)
  object.

#### Returns

An updated `ProjectProblem` object.

------------------------------------------------------------------------

### `ProjectProblem$add_objective()`

Create a new object with the objective added to the problem formulation.

#### Usage

    ProjectProblem$add_objective(x)

#### Arguments

- `x`:

  [Objective](https://prioritizr.github.io/oppr/reference/Objective-class.md)
  object.

#### Returns

An updated `ProjectProblem` object.

------------------------------------------------------------------------

### `ProjectProblem$add_decisions()`

Create a new object with the decisions added to the problem formulation.

#### Usage

    ProjectProblem$add_decisions(x)

#### Arguments

- `x`:

  [Objective](https://prioritizr.github.io/oppr/reference/Objective-class.md)
  object.

#### Returns

An updated `ProjectProblem` object.

------------------------------------------------------------------------

### `ProjectProblem$add_constraint()`

Create a new object with the constraint added to the problem
formulation.

#### Usage

    ProjectProblem$add_constraint(x)

#### Arguments

- `x`:

  [Constraint](https://prioritizr.github.io/oppr/reference/Constraint-class.md)
  object.

#### Returns

An updated `ProjectProblem` object.

------------------------------------------------------------------------

### `ProjectProblem$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProjectProblem$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
