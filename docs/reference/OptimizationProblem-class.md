# Optimization problem class

This class is used to represent an optimization problem. It stores the
information needed to generate a solution using an exact algorithm
solver. Most users should use
[`compile()`](https://prioritizr.github.io/oppr/reference/compile.md) to
generate new optimization problem objects, and the functions distributed
with the package to interact with them (e.g.,
[`base::as.list()`](https://rdrr.io/r/base/list.html)). **Only experts
should use the fields and methods for this class directly.**

## See also

Other classes:
[`Constraint-class`](https://prioritizr.github.io/oppr/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.github.io/oppr/reference/Decision-class.md),
[`MultiObjApproach-class`](https://prioritizr.github.io/oppr/reference/MultiObjApproach-class.md),
[`MultiObjProjectProblem-class`](https://prioritizr.github.io/oppr/reference/MultiObjProjectProblem-class.md),
[`Objective-class`](https://prioritizr.github.io/oppr/reference/Objective-class.md),
[`ProjectModifier-class`](https://prioritizr.github.io/oppr/reference/ProjectModifier-class.md),
[`ProjectProblem-class`](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md),
[`Solver-class`](https://prioritizr.github.io/oppr/reference/Solver-class.md),
[`Target-class`](https://prioritizr.github.io/oppr/reference/Target-class.md),
[`Weight-class`](https://prioritizr.github.io/oppr/reference/Weight-class.md)

## Public fields

- `ptr`:

  A `Rcpp::Xptr` external pointer.

- `data`:

  A `list` with supplemental data. Create a new optimization problem
  object.

## Methods

### Public methods

- [`OptimizationProblem$new()`](#method-OptimizationProblem-new)

- [`OptimizationProblem$get_data()`](#method-OptimizationProblem-get_data)

- [`OptimizationProblem$print()`](#method-OptimizationProblem-print)

- [`OptimizationProblem$show()`](#method-OptimizationProblem-show)

- [`OptimizationProblem$ncol()`](#method-OptimizationProblem-ncol)

- [`OptimizationProblem$nrow()`](#method-OptimizationProblem-nrow)

- [`OptimizationProblem$ncell()`](#method-OptimizationProblem-ncell)

- [`OptimizationProblem$modelsense()`](#method-OptimizationProblem-modelsense)

- [`OptimizationProblem$vtype()`](#method-OptimizationProblem-vtype)

- [`OptimizationProblem$obj()`](#method-OptimizationProblem-obj)

- [`OptimizationProblem$pwlobj()`](#method-OptimizationProblem-pwlobj)

- [`OptimizationProblem$A()`](#method-OptimizationProblem-A)

- [`OptimizationProblem$rhs()`](#method-OptimizationProblem-rhs)

- [`OptimizationProblem$sense()`](#method-OptimizationProblem-sense)

- [`OptimizationProblem$lb()`](#method-OptimizationProblem-lb)

- [`OptimizationProblem$ub()`](#method-OptimizationProblem-ub)

- [`OptimizationProblem$number_of_features()`](#method-OptimizationProblem-number_of_features)

- [`OptimizationProblem$number_of_branches()`](#method-OptimizationProblem-number_of_branches)

- [`OptimizationProblem$number_of_allocations()`](#method-OptimizationProblem-number_of_allocations)

- [`OptimizationProblem$number_of_actions()`](#method-OptimizationProblem-number_of_actions)

- [`OptimizationProblem$number_of_projects()`](#method-OptimizationProblem-number_of_projects)

- [`OptimizationProblem$col_ids()`](#method-OptimizationProblem-col_ids)

- [`OptimizationProblem$row_ids()`](#method-OptimizationProblem-row_ids)

- [`OptimizationProblem$copy()`](#method-OptimizationProblem-copy)

- [`OptimizationProblem$convert_pwlobj()`](#method-OptimizationProblem-convert_pwlobj)

- [`OptimizationProblem$clone()`](#method-OptimizationProblem-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    OptimizationProblem$new(ptr, data = list())

#### Arguments

- `ptr`:

  `Rcpp::Xptr` external pointer.

- `data`:

  `list` with supplemental data.

#### Returns

A new `OptimizationProblem` object.

------------------------------------------------------------------------

### Method `get_data()`

Obtain the supplemental data.

#### Usage

    OptimizationProblem$get_data()

#### Returns

A `list` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print concise information about the object.

#### Usage

    OptimizationProblem$print()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`show()`](https://prioritizr.github.io/oppr/reference/show.md)

Print concise information about the object.

#### Usage

    OptimizationProblem$show()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`ncol()`](https://prioritizr.github.io/oppr/reference/tibble-methods.md)

Obtain the number of columns in the problem formulation.

#### Usage

    OptimizationProblem$ncol()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method [`nrow()`](https://prioritizr.github.io/oppr/reference/tibble-methods.md)

Obtain the number of rows in the problem formulation.

#### Usage

    OptimizationProblem$nrow()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method `ncell()`

Obtain the number of cells in the problem formulation.

#### Usage

    OptimizationProblem$ncell()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method `modelsense()`

Obtain the model sense.

#### Usage

    OptimizationProblem$modelsense()

#### Returns

A `character` value.

------------------------------------------------------------------------

### Method `vtype()`

Obtain the decision variable types.

#### Usage

    OptimizationProblem$vtype()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### Method `obj()`

Obtain the objective function.

#### Usage

    OptimizationProblem$obj()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### Method `pwlobj()`

Obtain the piecewise linear components of the objective function.

#### Usage

    OptimizationProblem$pwlobj()

#### Returns

A `list` object.

------------------------------------------------------------------------

### Method `A()`

Obtain the constraint matrix.

#### Usage

    OptimizationProblem$A()

#### Returns

A
[`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
object.

------------------------------------------------------------------------

### Method `rhs()`

Obtain the right-hand-side constraint values.

#### Usage

    OptimizationProblem$rhs()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### Method `sense()`

Obtain the constraint senses.

#### Usage

    OptimizationProblem$sense()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### Method `lb()`

Obtain the lower bounds for the decision variables.

#### Usage

    OptimizationProblem$lb()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### Method `ub()`

Obtain the upper bounds for the decision variables.

#### Usage

    OptimizationProblem$ub()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### Method [`number_of_features()`](https://prioritizr.github.io/oppr/reference/number_of_features.md)

Obtain the number of features.

#### Usage

    OptimizationProblem$number_of_features()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method `number_of_branches()`

Obtain the number of phylogenetic branches.

#### Usage

    OptimizationProblem$number_of_branches()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method `number_of_allocations()`

Obtain the number of allocation variables. This number represents the
total number of decision variables used to identify if each project is
allocated to each variable.

#### Usage

    OptimizationProblem$number_of_allocations()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method [`number_of_actions()`](https://prioritizr.github.io/oppr/reference/number_of_actions.md)

Obtain the number of actions

#### Usage

    OptimizationProblem$number_of_actions()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method [`number_of_projects()`](https://prioritizr.github.io/oppr/reference/number_of_projects.md)

Obtain the number of projects.

#### Usage

    OptimizationProblem$number_of_projects()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method `col_ids()`

Obtain the identifiers for the columns.

#### Usage

    OptimizationProblem$col_ids()

#### Returns

A `character` value.

------------------------------------------------------------------------

### Method `row_ids()`

Obtain the identifiers for the rows.

#### Usage

    OptimizationProblem$row_ids()

#### Returns

A `character` value.

------------------------------------------------------------------------

### Method `copy()`

Copy the object.

#### Usage

    OptimizationProblem$copy()

#### Returns

An `OptimizationProblem` object.

------------------------------------------------------------------------

### Method `convert_pwlobj()`

Convert the piece-wise linear components of the objective function into
linear objective components and constraints.

#### Usage

    OptimizationProblem$convert_pwlobj()

#### Returns

An invisible `TRUE`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OptimizationProblem$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
