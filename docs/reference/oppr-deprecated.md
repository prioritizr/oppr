# Deprecation notice

The functions listed here are deprecated. This means that they once
existed in earlier versions of the of the oppr package, but they have
since been removed entirely, replaced by other functions, or renamed as
other functions in newer versions. To help make it easier to transition
to new versions of the oppr package, we have listed alternatives for
deprecated the functions (where applicable). If a function is described
as being renamed, then this means that only the name of the function has
changed (i.e., the inputs, outputs, and underlying code remain the
same).

## Usage

``` r
plot_feature_persistence(...)

plot_phylo_persistence(...)

add_locked_in_constraints(...)

add_locked_out_constraints(...)

add_manual_locked_constraints(...)
```

## Arguments

- ...:

  not used.

## Details

The following functions have been deprecated:

- [`add_max_richness_objective()`](https://prioritizr.github.io/oppr/reference/add_max_richness_objective.md):

  renamed as the
  [`add_max_wtd_sum_objective()`](https://prioritizr.github.io/oppr/reference/add_max_wtd_sum_objective.md)
  function.

- `plot_feature_persistence()`:

  renamed as the
  [`plot_solution_barplot()`](https://prioritizr.github.io/oppr/reference/plot_solution_barplot.md)
  function.

- `plot_phylo_persistence()`:

  renamed as the
  [`plot_solution_phylogram()`](https://prioritizr.github.io/oppr/reference/plot_solution_phylogram.md)
  function.

- `add_locked_in_constraints()`:

  renamed as the
  [`add_locked_in_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_in_action_constraints.md)
  function.

- `add_locked_out_constraints()`:

  renamed as the
  [`add_locked_out_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_out_action_constraints.md)
  function.

- `add_manual_locked_constraints()`:

  renamed as the
  [`add_manual_locked_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_manual_locked_action_constraints.md)
  function.
