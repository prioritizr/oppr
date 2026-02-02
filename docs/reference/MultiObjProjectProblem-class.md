# Multi-objective project problem class

This class is used to represent multi-objective project planning
problems. It stores the data (e.g., actions, and features) and
mathematical formulation (e.g., the objective, constraints, and other
design criteria) needed to generate prioritizations. Most users should
use
[`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
to generate new multi-objective project problem objects, and the
functions distributed with the package to interact with them. **Only
experts should use the fields and methods for this class directly.**

## See also

Other classes:
[`Constraint-class`](https://prioritizr.github.io/oppr/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.github.io/oppr/reference/Decision-class.md),
[`MultiObjApproach-class`](https://prioritizr.github.io/oppr/reference/MultiObjApproach-class.md),
[`Objective-class`](https://prioritizr.github.io/oppr/reference/Objective-class.md),
[`OptimizationProblem-class`](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md),
[`ProjectModifier-class`](https://prioritizr.github.io/oppr/reference/ProjectModifier-class.md),
[`ProjectProblem-class`](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md),
[`Solver-class`](https://prioritizr.github.io/oppr/reference/Solver-class.md),
[`Target-class`](https://prioritizr.github.io/oppr/reference/Target-class.md),
[`Weight-class`](https://prioritizr.github.io/oppr/reference/Weight-class.md)

## Public fields

- `problems`:

  `list` containing
  [`ProjectProblem`](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md)
  objects.

- `defaults`:

  `list` indicating if other fields contain defaults.

- `approach`:

  [`MultiObjApproach`](https://prioritizr.github.io/oppr/reference/MultiObjApproach-class.md)
  object for specifying the multi-objective optimization approach.

- `solver`:

  [`Solver`](https://prioritizr.github.io/oppr/reference/Solver-class.md)
  object specifying the solver for generating solutions.

## Methods

### Public methods

- [`MultiObjProjectProblem$new()`](#method-MultiObjProjectProblem-new)

- [`MultiObjProjectProblem$print()`](#method-MultiObjProjectProblem-print)

- [`MultiObjProjectProblem$show()`](#method-MultiObjProjectProblem-show)

- [`MultiObjProjectProblem$repr()`](#method-MultiObjProjectProblem-repr)

- [`MultiObjProjectProblem$number_of_problems()`](#method-MultiObjProjectProblem-number_of_problems)

- [`MultiObjProjectProblem$number_of_features()`](#method-MultiObjProjectProblem-number_of_features)

- [`MultiObjProjectProblem$number_of_actions()`](#method-MultiObjProjectProblem-number_of_actions)

- [`MultiObjProjectProblem$number_of_projects()`](#method-MultiObjProjectProblem-number_of_projects)

- [`MultiObjProjectProblem$problem_names()`](#method-MultiObjProjectProblem-problem_names)

- [`MultiObjProjectProblem$feature_names()`](#method-MultiObjProjectProblem-feature_names)

- [`MultiObjProjectProblem$action_names()`](#method-MultiObjProjectProblem-action_names)

- [`MultiObjProjectProblem$project_names()`](#method-MultiObjProjectProblem-project_names)

- [`MultiObjProjectProblem$add_approach()`](#method-MultiObjProjectProblem-add_approach)

- [`MultiObjProjectProblem$add_solver()`](#method-MultiObjProjectProblem-add_solver)

- [`MultiObjProjectProblem$clone()`](#method-MultiObjProjectProblem-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new multi-objective conservation problem object.

#### Usage

    MultiObjProjectProblem$new(problems)

#### Arguments

- `problems`:

  `list` containing
  [`ProjectProblem`](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md)
  objects.

#### Returns

A new `MultiObjProjectProblem` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print concise information about the object.

#### Usage

    MultiObjProjectProblem$print()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`show()`](https://prioritizr.github.io/oppr/reference/show.md)

Display concise information about the object.

#### Usage

    MultiObjProjectProblem$show()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `repr()`

Generate a character representation of the object.

#### Usage

    MultiObjProjectProblem$repr()

#### Returns

A `character` value.

------------------------------------------------------------------------

### Method [`number_of_problems()`](https://prioritizr.github.io/oppr/reference/number_of_problems.md)

Obtain the number of problems.

#### Usage

    MultiObjProjectProblem$number_of_problems()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### Method [`number_of_features()`](https://prioritizr.github.io/oppr/reference/number_of_features.md)

Obtain the number of features.

#### Usage

    MultiObjProjectProblem$number_of_features()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### Method [`number_of_actions()`](https://prioritizr.github.io/oppr/reference/number_of_actions.md)

Obtain the number of actions.

#### Usage

    MultiObjProjectProblem$number_of_actions()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### Method [`number_of_projects()`](https://prioritizr.github.io/oppr/reference/number_of_projects.md)

Obtain the number of projects.

#### Usage

    MultiObjProjectProblem$number_of_projects()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### Method [`problem_names()`](https://prioritizr.github.io/oppr/reference/problem_names.md)

Obtain the names of the problems.

#### Usage

    MultiObjProjectProblem$problem_names()

#### Returns

An `character` value.

------------------------------------------------------------------------

### Method [`feature_names()`](https://prioritizr.github.io/oppr/reference/feature_names.md)

Obtain the names of the features.

#### Usage

    MultiObjProjectProblem$feature_names()

#### Returns

A `list` of `character` vectors.

------------------------------------------------------------------------

### Method [`action_names()`](https://prioritizr.github.io/oppr/reference/action_names.md)

Obtain the names of the actions.

#### Usage

    MultiObjProjectProblem$action_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### Method [`project_names()`](https://prioritizr.github.io/oppr/reference/project_names.md)

Obtain the names of the projects.

#### Usage

    MultiObjProjectProblem$project_names()

#### Returns

A `list` of `character` vectors.

------------------------------------------------------------------------

### Method `add_approach()`

Create a new object with an approach added to the problem formulation.

#### Usage

    MultiObjProjectProblem$add_approach(x)

#### Arguments

- `x`:

  [MultiObjApproach](https://prioritizr.github.io/oppr/reference/MultiObjApproach-class.md)
  object.

#### Returns

An updated `MultiObjProjectProblem` object.

------------------------------------------------------------------------

### Method `add_solver()`

Create a new object with a solver added to the problem formulation.

#### Usage

    MultiObjProjectProblem$add_solver(x)

#### Arguments

- `x`:

  [Solver](https://prioritizr.github.io/oppr/reference/Solver-class.md)
  object.

#### Returns

An updated `MultiObjProjectProblem` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MultiObjProjectProblem$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
