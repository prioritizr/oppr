# Conservation problem modifier class

This super-class is used to represent prototypes that in turn are used
to modify a
[ProjectProblem](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md)
object. Specifically, the
[Constraint](https://prioritizr.github.io/oppr/reference/Constraint-class.md),
[Decision](https://prioritizr.github.io/oppr/reference/Decision-class.md),
[Objective](https://prioritizr.github.io/oppr/reference/Objective-class.md),
and
[Target](https://prioritizr.github.io/oppr/reference/Target-class.md)
prototypes inherit from this class. **Only experts should use the fields
and methods for this class directly.**

## See also

Other classes:
[`Constraint-class`](https://prioritizr.github.io/oppr/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.github.io/oppr/reference/Decision-class.md),
[`MultiObjApproach-class`](https://prioritizr.github.io/oppr/reference/MultiObjApproach-class.md),
[`MultiObjProjectProblem-class`](https://prioritizr.github.io/oppr/reference/MultiObjProjectProblem-class.md),
[`Objective-class`](https://prioritizr.github.io/oppr/reference/Objective-class.md),
[`OptimizationProblem-class`](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md),
[`ProjectProblem-class`](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md),
[`Solver-class`](https://prioritizr.github.io/oppr/reference/Solver-class.md),
[`Target-class`](https://prioritizr.github.io/oppr/reference/Target-class.md),
[`Weight-class`](https://prioritizr.github.io/oppr/reference/Weight-class.md)

## Public fields

- `name`:

  `character` value.

- `data`:

  `list` containing data.

- `internal`:

  `list` containing internal computed values.

## Methods

### Public methods

- [`ProjectModifier$print()`](#method-ProjectModifier-print)

- [`ProjectModifier$show()`](#method-ProjectModifier-show)

- [`ProjectModifier$repr()`](#method-ProjectModifier-repr)

- [`ProjectModifier$get_data()`](#method-ProjectModifier-get_data)

- [`ProjectModifier$set_data()`](#method-ProjectModifier-set_data)

- [`ProjectModifier$get_internal()`](#method-ProjectModifier-get_internal)

- [`ProjectModifier$set_internal()`](#method-ProjectModifier-set_internal)

- [`ProjectModifier$calculate()`](#method-ProjectModifier-calculate)

- [`ProjectModifier$apply()`](#method-ProjectModifier-apply)

- [`ProjectModifier$clone()`](#method-ProjectModifier-clone)

------------------------------------------------------------------------

### `ProjectModifier$print()`

Print information about the object.

#### Usage

    ProjectModifier$print()

#### Returns

None.

------------------------------------------------------------------------

### `ProjectModifier$show()`

Print information about the object.

#### Usage

    ProjectModifier$show()

#### Returns

None.

------------------------------------------------------------------------

### `ProjectModifier$repr()`

Generate a character representation of the object.

#### Usage

    ProjectModifier$repr()

#### Returns

A `character` value.

------------------------------------------------------------------------

### `ProjectModifier$get_data()`

Get values stored in the `data` field.

#### Usage

    ProjectModifier$get_data(x)

#### Arguments

- `x`:

  `character` name of data.

#### Returns

An object. If the `data` field does not contain an object associated
with the argument to `x`, then a
[`new_waiver()`](https://prioritizr.github.io/oppr/reference/new_waiver.md)
object is returned. Set values stored in the `data` field. Note that
this method will overwrite existing data.

------------------------------------------------------------------------

### `ProjectModifier$set_data()`

#### Usage

    ProjectModifier$set_data(x, value)

#### Arguments

- `x`:

  `character` name of data.

- `value`:

  Object to store.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `ProjectModifier$get_internal()`

Get values stored in the `internal` field.

#### Usage

    ProjectModifier$get_internal(x)

#### Arguments

- `x`:

  `character` name of data.

#### Returns

An object. If the `internal` field does not contain an object associated
with the argument to `x`, then a
[`new_waiver()`](https://prioritizr.github.io/oppr/reference/new_waiver.md)
object is returned.

------------------------------------------------------------------------

### `ProjectModifier$set_internal()`

Set values stored in the `internal` field. Note that this method will
overwrite existing data.

#### Usage

    ProjectModifier$set_internal(x, value)

#### Arguments

- `x`:

  `character` name of data.

- `value`:

  Object to store.

#### Returns

An object. If the `internal` field does not contain an object associated
with the argument to `x`, then a
[`new_waiver()`](https://prioritizr.github.io/oppr/reference/new_waiver.md)
object is returned.

------------------------------------------------------------------------

### `ProjectModifier$calculate()`

Perform computations that need to be completed before applying the
object.

#### Usage

    ProjectModifier$calculate(x, y)

#### Arguments

- `x`:

  [`new_optimization_problem()`](https://prioritizr.github.io/oppr/reference/new_optimization_problem.md)
  object.

- `y`:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  object.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `ProjectModifier$apply()`

Update an optimization problem formulation.

#### Usage

    ProjectModifier$apply(x)

#### Arguments

- `x`:

  [`new_optimization_problem()`](https://prioritizr.github.io/oppr/reference/new_optimization_problem.md)
  object.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `ProjectModifier$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProjectModifier$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
