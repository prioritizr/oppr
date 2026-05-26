# Number of features

Get the number of features in an object.

## Usage

``` r
number_of_features(x)

# S4 method for class 'ProjectProblem'
number_of_features(x)

# S4 method for class 'MultiObjProjectProblem'
number_of_features(x)
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
data(sim_projects, sim_features, sim_actions)

# build problem
p <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_max_wtd_sum_objective(budget = 200) %>%
  add_binary_decisions() %>%
  add_default_solver()

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
#> solver:          gurobi solver

# print number of features
number_of_features(p)
#> [1] 5
```
