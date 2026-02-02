# Target class

This class is used to represent targets for optimization. **Only experts
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
[`Solver-class`](https://prioritizr.github.io/oppr/reference/Solver-class.md),
[`Weight-class`](https://prioritizr.github.io/oppr/reference/Weight-class.md)

## Super class

[`oppr::ProjectModifier`](https://prioritizr.github.io/oppr/reference/ProjectModifier-class.md)
-\> `Target`

## Methods

### Public methods

- [`Target$output()`](#method-Target-output)

- [`Target$clone()`](#method-Target-clone)

Inherited methods

- [`oppr::ProjectModifier$apply()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-apply)
- [`oppr::ProjectModifier$calculate()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-calculate)
- [`oppr::ProjectModifier$get_data()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-get_data)
- [`oppr::ProjectModifier$get_internal()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-get_internal)
- [`oppr::ProjectModifier$print()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-print)
- [`oppr::ProjectModifier$repr()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-repr)
- [`oppr::ProjectModifier$set_data()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-set_data)
- [`oppr::ProjectModifier$set_internal()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-set_internal)
- [`oppr::ProjectModifier$show()`](https://prioritizr.github.io/oppr/reference/ProjectModifier.html#method-show)

------------------------------------------------------------------------

### Method `output()`

Output the targets.

#### Usage

    Target$output()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Target$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
