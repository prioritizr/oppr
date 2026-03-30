# Rank importance

Calculate the rank importance for projects in a project prioritization
[`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
(Jung *et al.* 2021). Projects associated with a higher rank value are
more irreplaceable, and may need to be implemented sooner than those
with lower values.

## Usage

``` r
rank_importance(x, solution, n = 1, ranks = 10, budgets = NULL, ...)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  or
  [`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
  object.

- solution:

  [`base::data.frame()`](https://rdrr.io/r/base/data.frame.html) or
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing the solutions. Here, rows correspond to different solutions
  and columns correspond to different actions. Each column in the
  argument to `solution` should be named according to a different action
  in `x`. Cell values indicate if an action is funded in a given
  solution or not, and should be either zero or one. Arguments to
  `solution` can contain additional columns, though they will be
  ignored.

- n:

  `integer` solution number to calculate replacement cost values. Since
  each row in the argument to `solutions` corresponds to a different
  solution, this argument should correspond to a row in the argument to
  `solutions`. Defaults to 1.

- ranks:

  `integer` number to incremental ranks to evaluate importance. Defaults
  to 10.

- budgets:

  `numeric` budget values for generating prioritizations at each
  increment. This parameter can be used instead of `ranks` to specify
  the number of incremental ranks and also the budget values that should
  be considered for each rank. Defaults to `NULL`.

- ...:

  Arguments passed to
  [`solve()`](https://prioritizr.github.io/oppr/reference/solve.md).

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
table containing the following columns.

- `"project"`:

  `character` name of each project.

- `"rank"`:

  `integer` rank where the project was first selected.

- `"score"`:

  `numeric` importance score.

## Details

This method involves generating a series of incremental prioritizations,
that start with relatively few projects selected and then iteratively
selecting additional projects. Projects that are selected at the first
increment are assigned the highest importance score and those selected
in subsequent increments are assigned lower importance score. Missing
(`NA`) values are assigned to projects which are not selected for
funding in the specified solution.

## References

Jung M, Arnell A, de Lamo X, et al. (2021) Areas of global importance
for conserving terrestrial biodiversity, carbon and water. *Nature
Ecology and Evolution*, **5**, 1499–1509.

## See also

Other functions for evaluating solutions:
[`project_cost_effectiveness()`](https://prioritizr.github.io/oppr/reference/project_cost_effectiveness.md),
[`replacement_costs()`](https://prioritizr.github.io/oppr/reference/replacement_costs.md),
[`solution_statistics()`](https://prioritizr.github.io/oppr/reference/solution_statistics.md)

## Examples

``` r
# \dontrun{
# load data
data(sim_projects, sim_features, sim_actions)

# build problem with maximum weighted sum objective and $400 budget
p <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_max_wtd_sum_objective(budget = 400) %>%
  add_feature_weights("weight") %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s <- solve(p)

# print solution
print(s)
#> # A tibble: 1 × 21
#>   solution status   cost   obj F1_action F2_action F3_action F4_action F5_action
#>      <int> <chr>   <dbl> <dbl> <lgl>     <lgl>     <lgl>     <lgl>     <lgl>    
#> 1        1 OPTIMAL  395.  1.75 TRUE      TRUE      FALSE     TRUE      TRUE     
#> # ℹ 12 more variables: baseline_action <lgl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>

# calculate rank importance values
r <- rank_importance(p, s)

# print output
print(r)
#> # A tibble: 6 × 3
#>   project           rank  score
#>   <chr>            <dbl>  <dbl>
#> 1 F1_project           5  0.556
#> 2 F2_project          10  0    
#> 3 F3_project          NA NA    
#> 4 F4_project           8  0.222
#> 5 F5_project           3  0.778
#> 6 baseline_project     1  1    
# }
```
