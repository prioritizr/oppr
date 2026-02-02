# Solution statistics

Calculate statistics to describe a solution to a project prioritization
problem.

## Usage

``` r
solution_statistics(x, solution)
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

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
containing the following columns.

- `"cost"`:

  This column contains `numeric` values describing the cost of each
  solution.

- `"obj"`:

  This column contains `numeric` values describing the objective value
  for each solution. This is calculated using the objective function
  defined for the argument to `x`. Note that if `x` is a
  [`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
  object, then an objective column will be created for each problem in
  `x`.

- `x$project_names()`:

  These columns contain `logical` values that indicate if each project
  had all of its actions selected for funding or not.

- `x$feature_names()`:

  These columns contain `numeric` values that describe the expected
  outcome for each feature based on the actions selected for funding.

## See also

Other functions for evaluating solutions:
[`project_cost_effectiveness()`](https://prioritizr.github.io/oppr/reference/project_cost_effectiveness.md),
[`rank_importance()`](https://prioritizr.github.io/oppr/reference/rank_importance.md),
[`replacement_costs()`](https://prioritizr.github.io/oppr/reference/replacement_costs.md)

## Examples

``` r
# load data
data(sim_projects, sim_features, sim_actions)

# print project data
print(sim_projects)
#> # A tibble: 6 × 13
#>   name           success     F1     F2      F3     F4     F5 F1_action F2_action
#>   <chr>            <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl> <lgl>     <lgl>    
#> 1 F1_project       0.919  0.791 NA     NA      NA     NA     TRUE      FALSE    
#> 2 F2_project       0.923 NA      0.888 NA      NA     NA     FALSE     TRUE     
#> 3 F3_project       0.829 NA     NA      0.502  NA     NA     FALSE     FALSE    
#> 4 F4_project       0.848 NA     NA     NA       0.690 NA     FALSE     FALSE    
#> 5 F5_project       0.814 NA     NA     NA      NA      0.617 FALSE     FALSE    
#> 6 baseline_proj…   1      0.298  0.250  0.0865  0.249  0.182 FALSE     FALSE    
#> # ℹ 4 more variables: F3_action <lgl>, F4_action <lgl>, F5_action <lgl>,
#> #   baseline_action <lgl>

# print action data
print(sim_features)
#> # A tibble: 5 × 2
#>   name  weight
#>   <chr>  <dbl>
#> 1 F1     0.211
#> 2 F2     0.211
#> 3 F3     0.221
#> 4 F4     0.630
#> 5 F5     1.59 

# print feature data
print(sim_actions)
#> # A tibble: 6 × 4
#>   name             cost locked_in locked_out
#>   <chr>           <dbl> <lgl>     <lgl>     
#> 1 F1_action        94.4 FALSE     FALSE     
#> 2 F2_action       101.  FALSE     FALSE     
#> 3 F3_action       103.  TRUE      FALSE     
#> 4 F4_action        99.2 FALSE     FALSE     
#> 5 F5_action        99.9 FALSE     TRUE      
#> 6 baseline_action   0   FALSE     FALSE     

# build problem
p <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_max_wtd_sum_objective(budget = 400) %>%
  add_feature_weights("weight") %>%
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
#> weights:         feature weights
#> constraints:     none specified
#> decisions:       binary decision
#> solver:          none specified

# create a table with some solutions
solutions <- data.frame(
  F1_action = c(0, 1, 1),
  F2_action = c(0, 1, 0),
  F3_action = c(0, 1, 1),
  F4_action = c(0, 1, 0),
  F5_action = c(0, 1, 1),
  baseline_action = c(1, 1, 1)
)

# print the solutions
# the first solution only has the baseline action funded
# the second solution has every action funded
# the third solution has only some actions funded
print(solutions)
#>   F1_action F2_action F3_action F4_action F5_action baseline_action
#> 1         0         0         0         0         0               1
#> 2         1         1         1         1         1               1
#> 3         1         0         1         0         1               1

# calculate statistics for the solutions
solution_statistics(p, solutions)
#> # A tibble: 3 × 13
#>    cost   obj F1_project F2_project F3_project F4_project F5_project
#>   <dbl> <dbl> <lgl>      <lgl>      <lgl>      <lgl>      <lgl>     
#> 1    0  0.581 FALSE      FALSE      FALSE      FALSE      FALSE     
#> 2  498. 1.83  TRUE       TRUE       TRUE       TRUE       TRUE      
#> 3  298. 1.43  TRUE       FALSE      TRUE       FALSE      TRUE      
#> # ℹ 6 more variables: baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>,
#> #   F4 <dbl>, F5 <dbl>
```
