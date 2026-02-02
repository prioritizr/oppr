# Number of problems

Get the number of problems in an object.

## Usage

``` r
number_of_problems(x)

# S4 method for class 'MultiObjProjectProblem'
number_of_problems(x)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  object.

## Value

An `integer` value.

## Examples

``` r
# load data
data(sim_multi_projects)
data(sim_multi_features)
data(sim_multi_actions)
data(sim_multi_tree)

# build problem
p <-
  multi_problem(
    obj1 =
      problem(
        sim_multi_projects[[1]], sim_multi_actions, sim_multi_features[[1]],
        "name", "success", "name", "cost", "name",
        baseline_project_name = "baseline_project_obj1"
      ) %>%
      add_max_phylo_div_objective(
        budget = 200, tree = sim_multi_tree[[1]]
      ) %>%
      add_binary_decisions(),
   obj2 =
     problem(
       sim_multi_projects[[2]], sim_multi_actions, sim_multi_features[[2]],
       "name", "success", "name", "cost", "name",
       baseline_project_name = "baseline_project_obj2"
     ) %>%
     add_max_richness_objective(budget = 200) %>%
     add_binary_decisions(),
   obj3 =
     problem(
       sim_multi_projects[[3]], sim_multi_actions, sim_multi_features[[3]],
       "name", "success", "name", "cost", "name",
       baseline_project_name = "baseline_project_obj3"
     ) %>%
     add_max_wtd_sum_objective(budget = 200) %>%
     add_binary_decisions()
 )

# print problem
print(p)
#> Multi-objective Project Prioritization Problem
#> objective:         obj1
#>   projects:        F1_project, F2_project, F8_project, baseline_project_obj1 (4 projects)
#>   features:        F1, F2, F8 (3 features)
#>   project success: proportion values (between 0.832 and 1)
#>   objective:       maximum phylogenetic diversity objective
#>   targets:         none specified
#>   weights:         none specified
#>   constraints:     none specified
#>   decisions:       binary decision
#> objective:         obj2
#>   projects:        F3_project, F4_project, baseline_project_obj2 (3 projects)
#>   features:        F3, F4 (2 features)
#>   project success: proportion values (between 0.85 and 1)
#>   objective:       maximum richness objective
#>   targets:         none specified
#>   weights:         none specified
#>   constraints:     none specified
#>   decisions:       binary decision
#> objective:         obj3
#>   projects:        F5_project, F6_project, F7_project, ... (6 projects)
#>   features:        F5, F6, F7, ... (5 features)
#>   project success: proportion values (between 0.715 and 1)
#>   objective:       maximum weighted sum objective
#>   targets:         none specified
#>   weights:         none specified
#>   constraints:     none specified
#>   decisions:       binary decision
#> actions:           A1_action, A2_action, A3_action, ... (18 actions)
#> action costs:      continuous values (between 0 and 103.226)
#> approach:          none specified
#> solver:            none specified

# print number of problems
number_of_problems(p)
#> [1] 3
```
