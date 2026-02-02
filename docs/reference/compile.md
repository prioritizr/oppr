# Compile a problem

Compile a project prioritization
[`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
into a general purpose format for optimization.

## Usage

``` r
compile(x, ...)

# S3 method for class 'ProjectProblem'
compile(x, n_approx = 100, ...)

# S3 method for class 'MultiObjProjectProblem'
multi_compile(x, ...)

# S3 method for class 'list'
multi_compile(x, ...)
```

## Arguments

- x:

  [ProjectProblem](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md)
  object.

- ...:

  not used.

- n_approx:

  `integer` number of points to use for piece-wise linear approximations
  of non-linear terms. Defaults to 100.

## Value

An
[OptimizationProblem](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md)
object.

## Details

This function might be useful for those interested in understanding how
their project prioritization
[`problem()`](https://prioritizr.github.io/oppr/reference/problem.md) is
expressed as a mathematical problem. However, if the problem just needs
to be solved, then the
[`solve()`](https://prioritizr.github.io/oppr/reference/solve.md)
function should be used instead.

## Examples

``` r
# load data
data(sim_projects, sim_features, sim_actions)

# build problem with maximum weighted sum objective, $200 budget, and
# binary decisions
p <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_max_wtd_sum_objective(budget = 200) %>%
  add_binary_decisions()

# print problem
print(p)
#> Project Prioritization Problem
#> actions:         F1_action, F2_action, F3_action, ... (6 actions)
#> projects:        F1_project, F2_project, F3_project, ... (6 projects)
#> features:        F1, F2, F3, ... (5 features)
#> action costs:    continuous values (between 0 and 103.226)
#> project success: proportion values (between 0.814 and 1)
#> objective:       maximum weighted sum objective
#> targets:         none specified
#> weights:         none specified
#> constraints:     none specified
#> decisions:       binary decision
#> solver:          none specified

# compile problem
o <- compile(p)

# print compiled problem
print(o)
#> optimization problem
#>   objective:   linear
#>   model sense: max
#>   dimensions:  27, 27, 63 (nrow, ncol, ncell)
#>   variables:   22 (B), 5 (C)
```
