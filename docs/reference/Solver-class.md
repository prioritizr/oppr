# Solver class

This class is used to represent solvers for optimization. **Only experts
should use the fields and methods for this class directly.**

## See also

Other classes:
[`Constraint-class`](https://prioritizr.github.io/oppr/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.github.io/oppr/reference/Decision-class.md),
[`MultiObjApproach-class`](https://prioritizr.github.io/oppr/reference/MultiObjApproach-class.md),
[`MultiObjProjectProblem-class`](https://prioritizr.github.io/oppr/reference/MultiObjProjectProblem-class.md),
[`Objective-class`](https://prioritizr.github.io/oppr/reference/Objective-class.md),
[`OptimizationProblem-class`](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md),
[`ProjectModifier-class`](https://prioritizr.github.io/oppr/reference/ProjectModifier-class.md),
[`ProjectProblem-class`](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md),
[`Target-class`](https://prioritizr.github.io/oppr/reference/Target-class.md),
[`Weight-class`](https://prioritizr.github.io/oppr/reference/Weight-class.md)

## Super class

[`ProjectModifier`](https://prioritizr.github.io/oppr/reference/ProjectModifier-class.md)
-\> `Solver`

## Public fields

- `has_pwlobj`:

  `logical` indicating if solver supports piece-wise linear components
  in an objective function.

## Methods

### Public methods

- [`Solver$set_start_solution()`](#method-Solver-set_start_solution)

- [`Solver$remove_start_solution()`](#method-Solver-remove_start_solution)

- [`Solver$solve()`](#method-Solver-solve)

- [`Solver$clone()`](#method-Solver-clone)

Inherited methods

- [`ProjectModifier$apply()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-apply)
- [`ProjectModifier$calculate()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-calculate)
- [`ProjectModifier$get_data()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-get_data)
- [`ProjectModifier$get_internal()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-get_internal)
- [`ProjectModifier$print()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-print)
- [`ProjectModifier$repr()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-repr)
- [`ProjectModifier$set_data()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-set_data)
- [`ProjectModifier$set_internal()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-set_internal)
- [`ProjectModifier$show()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-show)

------------------------------------------------------------------------

### `Solver$set_start_solution()`

Set start solution.

#### Usage

    Solver$set_start_solution(x)

#### Arguments

- `x`:

  `numeric` vector.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `Solver$remove_start_solution()`

Remove start solution.

#### Usage

    Solver$remove_start_solution(x)

#### Arguments

- `x`:

  `numeric` vector.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `Solver$solve()`

Solve an optimization problem.

#### Usage

    Solver$solve(x, ...)

#### Arguments

- `x`:

  [`new_optimization_problem()`](https://prioritizr.github.io/oppr/reference/new_optimization_problem.md)
  object.

- `...`:

  Additional arguments as needed.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `Solver$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Solver$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
