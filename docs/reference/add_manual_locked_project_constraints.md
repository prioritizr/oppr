# Add manually specified locked constraints for projects

Add constraints to a project prioritization problem to ensure that
particular projects are selected, or not selected, for funding by the
solution. This function offers more fine-grained control than the
[`add_locked_in_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_in_project_constraints.md)
and
[`add_locked_out_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_out_project_constraints.md)
functions.

## Usage

``` r
add_manual_locked_project_constraints(x, locked)

# S4 method for class 'ProjectProblem,data.frame'
add_manual_locked_project_constraints(x, locked)

# S4 method for class 'ProjectProblem,tbl_df'
add_manual_locked_project_constraints(x, locked)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  object.

- locked:

  `data.frame` or
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object. See the Details section for more information.

## Value

A [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
object with the constraints added to it.

## Details

The argument to `locked` must contain the following columns.

- `"project"`:

  `character` values with project names.

- `"status"`:

  `numeric` values indicating if projects should be selected for funding
  (with a value of 1) or not (with a value of zero).

## See also

Other constraints:
[`add_locked_in_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_in_action_constraints.md),
[`add_locked_in_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_in_project_constraints.md),
[`add_locked_out_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_out_action_constraints.md),
[`add_locked_out_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_out_project_constraints.md),
[`add_manual_locked_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_manual_locked_action_constraints.md)

## Examples

``` r
# \dontrun{
# load data
data(sim_projects, sim_features, sim_actions)

# create data frame with locked statuses
locked_data <- data.frame(
  project = sim_projects$name[1:2],
  status = c(0, 1)
)

# print locked statuses
print(locked_data)
#>      project status
#> 1 F1_project      0
#> 2 F2_project      1

# build problem with minimum set objective and targets that require each
# feature to have a 30% chance of persisting into the future
p <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_max_wtd_sum_objective(budget = 500) %>%
  add_manual_locked_project_constraints(locked_data) %>%
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
#> constraints:     manually locked projects
#> decisions:       binary decision
#> solver:          none specified

# solve problem
s <- solve(p)
#> Set parameter Username
#> Set parameter LicenseID to value 2774703
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0
#> Set parameter ScaleFlag to value 2
#> Set parameter NumericFocus to value 1
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter PoolSolutions to value 1
#> Set parameter PoolSearchMode to value 2
#> Academic license - for non-commercial use only - expires 2027-02-03
#> Warning: Gurobi version mismatch between R 13.0.0 and C library 13.0.1
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0
#> ScaleFlag  2
#> NumericFocus  1
#> Presolve  2
#> Threads  1
#> PoolSolutions  1
#> PoolSearchMode  2
#> 
#> Optimize a model with 27 rows, 27 columns and 62 nonzeros (Max)
#> Model fingerprint: 0x2c897167
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 5e+02]
#> 
#> Found heuristic solution: objective 2.1193540
#> Presolve removed 21 rows and 16 columns
#> Presolve time: 0.00s
#> Presolved: 6 rows, 11 columns, 12 nonzeros
#> Variable types: 0 continuous, 11 integer (11 binary)
#> Root relaxation presolved: 6 rows, 11 columns, 12 nonzeros
#> 
#> 
#> Root relaxation: objective 2.910246e+00, 6 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       2.9102457    2.91025  0.00%     -    0s
#> 
#> Explored 1 nodes (6 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 2.91025 
#> No other solutions better than 2.91025
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.910245655750e+00, best bound 2.910245655750e+00, gap 0.0000%

# print solution
print(s)
#> # A tibble: 1 × 21
#>   solution status   cost   obj F1_action F2_action F3_action F4_action F5_action
#>      <int> <chr>   <dbl> <dbl> <lgl>     <lgl>     <lgl>     <lgl>     <lgl>    
#> 1        1 OPTIMAL  403.  2.91 FALSE     TRUE      TRUE      TRUE      TRUE     
#> # ℹ 12 more variables: baseline_action <lgl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>
# }
```
