# Objective class

This class is used to represent the objective function used in
optimization. **Only experts should use the fields and methods for this
class directly.**

## See also

Other classes:
[`Constraint-class`](https://prioritizr.github.io/oppr/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.github.io/oppr/reference/Decision-class.md),
[`MultiObjApproach-class`](https://prioritizr.github.io/oppr/reference/MultiObjApproach-class.md),
[`MultiObjProjectProblem-class`](https://prioritizr.github.io/oppr/reference/MultiObjProjectProblem-class.md),
[`OptimizationProblem-class`](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md),
[`ProjectModifier-class`](https://prioritizr.github.io/oppr/reference/ProjectModifier-class.md),
[`ProjectProblem-class`](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md),
[`Solver-class`](https://prioritizr.github.io/oppr/reference/Solver-class.md),
[`Target-class`](https://prioritizr.github.io/oppr/reference/Target-class.md),
[`Weight-class`](https://prioritizr.github.io/oppr/reference/Weight-class.md)

## Super class

[`oppr::ProjectModifier`](https://prioritizr.github.io/oppr/reference/ProjectModifier-class.md)
-\> `Objective`

## Public fields

- `has_targets`:

  `logical` value indicating if the objective uses targets.

- `has_weights`:

  `logical` value indicating if the objective uses weights.

## Methods

### Public methods

- [`Objective$feature_phylogeny()`](#method-Objective-feature_phylogeny)

- [`Objective$default_feature_weights()`](#method-Objective-default_feature_weights)

- [`Objective$replace_feature_weights()`](#method-Objective-replace_feature_weights)

- [`Objective$evaluate()`](#method-Objective-evaluate)

- [`Objective$clone()`](#method-Objective-clone)

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

### Method `feature_phylogeny()`

Obtain the feature phylogeny.

#### Usage

    Objective$feature_phylogeny()

#### Returns

A [`ape::phylo()`](https://rdrr.io/pkg/ape/man/read.tree.html)
phylogenetic tree object.

------------------------------------------------------------------------

### Method `default_feature_weights()`

Obtain default feature weights.

#### Usage

    Objective$default_feature_weights()

#### Returns

A `numeric` vector with the default feature weights.

------------------------------------------------------------------------

### Method `replace_feature_weights()`

Should default feature weights be replaced or multiplied by the new
weights?

#### Usage

    Objective$replace_feature_weights()

#### Returns

A `logical` value.ks

------------------------------------------------------------------------

### Method `evaluate()`

Calculate the objective value for a solution.

#### Usage

    Objective$evaluate(y, solution)

#### Arguments

- `y`:

  [ProjectProblem](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md)
  object.

- `solution`:

  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object with solution.

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Objective$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
