# Solve

Solve a conservation planning
[`problem()`](https://prioritizr.github.io/oppr/reference/problem.md).

## Usage

``` r
# S4 method for class 'OptimizationProblem,Solver'
solve(a, b, ...)

# S4 method for class 'ProjectProblem,missing'
solve(a, b, ...)

# S4 method for class 'MultiObjProjectProblem,missing'
solve(a, b, ...)
```

## Arguments

- a:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md),
  [`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md),
  or
  [OptimizationProblem](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md)
  object.

- b:

  [Solver](https://prioritizr.github.io/oppr/reference/Solver-class.md)
  object. Note that this parameter is only used if `a` is an
  [OptimizationProblem](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md)
  object.

- ...:

  arguments passed to
  [`compile()`](https://prioritizr.github.io/oppr/reference/compile.md).

## Value

The type of object returned from this function depends on the argument
to `a`. If the argument to `a` is an
[OptimizationProblem](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md)
object, then the solution is returned as a `list` containing the
prioritization and additional information (e.g., run time, solver
status). On the other hand, if the argument to `a` is a
[`problem()`](https://prioritizr.github.io/oppr/reference/problem.md) or
[`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
object, then a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object will be returned. In this table, each row row corresponds to a
different solution and each column describes a different property or
result associated with each solution. In particular, it will have the
following columns.

- `"solution"`:

  This column contains `integer` identifiers for the solutions.

- `"status"`:

  This column contains `character` values that describe the solver
  status. For example, these values may indicate if the solver returned
  an optimal or suboptimal solution.

- `"obj"`:

  This column contains `numeric` values that contain the objective value
  for each solution. This is calculated using the objective function
  defined for the argument to `a`. Note that if `a` is a
  [`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
  object, then an objective column will be created for each problem in
  `a`.

- `"cost"`:

  This column contains `numeric` values that describe the total cost
  associated with each solution.

- `x$action_names()`:

  These columns contain `logical` (`TRUE`/`FALSE`) values that indicate
  if each for each action was selected for funding (or not) by each
  solution.

- `x$project_names()`:

  These columns contain `logical` (`TRUE`/`FALSE`) values that indicate
  if each for each project had all of its actions selected for funding
  (or not) by each solution.

- `x$feature_names()`:

  `These columns contain`numeric\` values that describe the expected
  outcome for each feature based on the actions selected for funding.

## See also

The
[`solution_statistics()`](https://prioritizr.github.io/oppr/reference/solution_statistics.md)
function can be used to compute these statistics for solutions. This may
be useful to evaluate the performance of solutions generated based on
expert opinion, or solutions according to objectives that are different
from those used to generate them.

## Examples

``` r
# \dontrun{
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

# solve problem
s <- solve(p)
#> Set parameter Username
#> Set parameter LicenseID to value 2738655
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0
#> Set parameter NumericFocus to value 2
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter PoolSolutions to value 1
#> Set parameter PoolSearchMode to value 2
#> Academic license - for non-commercial use only - expires 2026-11-14
#> Gurobi Optimizer version 13.0.0 build v13.0.0rc1 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0
#> NumericFocus  2
#> Presolve  2
#> Threads  1
#> PoolSolutions  1
#> PoolSearchMode  2
#> 
#> Optimize a model with 27 rows, 27 columns and 62 nonzeros (Max)
#> Model fingerprint: 0x85f2486a
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [2e-01, 2e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 4e+02]
#> Found heuristic solution: objective 0.6654645
#> Presolve removed 16 rows and 12 columns
#> Presolve time: 0.00s
#> Presolved: 11 rows, 15 columns, 24 nonzeros
#> Variable types: 0 continuous, 15 integer (15 binary)
#> Root relaxation presolved: 11 rows, 15 columns, 24 nonzeros
#> 
#> 
#> Root relaxation: objective 1.749045e+00, 11 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       1.7490448    1.74904  0.00%     -    0s
#> 
#> Explored 1 nodes (11 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 1.74904 
#> No other solutions better than 1.74904
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 1.749044775334e+00, best bound 1.749044775334e+00, gap 0.0000%

# print output
print(s)
#> # A tibble: 1 × 21
#>   solution status   cost   obj F1_action F2_action F3_action F4_action F5_action
#>      <int> <chr>   <dbl> <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#> 1        1 OPTIMAL  395.  1.75         1         1         0         1         1
#> # ℹ 12 more variables: baseline_action <dbl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>

# print the solver status
print(s$obj)
#> [1] 1.749045

# print the objective value
print(s$obj)
#> [1] 1.749045

# print the solution cost
print(s$cost)
#> [1] 394.5413

# print which actions are funded in the solution
s[, sim_actions$name, drop = FALSE]
#> # A tibble: 1 × 6
#>   F1_action F2_action F3_action F4_action F5_action baseline_action
#>       <dbl>     <dbl>     <dbl>     <dbl>     <dbl>           <dbl>
#> 1         1         1         0         1         1               1

# print the expected probability of persistence for each feature
# if the solution were implemented
s[, sim_features$name, drop = FALSE]
#> # A tibble: 1 × 5
#>      F1    F2     F3    F4    F5
#>   <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1 0.808 0.865 0.0865 0.688 0.592
# }
```
