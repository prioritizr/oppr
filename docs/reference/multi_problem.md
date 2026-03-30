# Multi-objective project prioritization problem

Create a multi-objective systematic project prioritization problem.

## Usage

``` r
multi_problem(..., problem_names = NULL)
```

## Arguments

- ...:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  objects.

- problem_names:

  `character` vector with a name for each problem in `...`. Defaults to
  `NULL`, such that the problem names are defined automatically.

## Value

A
[MultiObjProjectProblem](https://prioritizr.github.io/oppr/reference/MultiObjProjectProblem-class.md)
object.

## Details

A multi-objective project prioritization problem contains multiple
single-objective project prioritization problems (i.e., created with
[`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)).
Each of these single-objective project prioritization problems must have
exactly the same actions (i.e., argument to `actions`). Additionally,
each single-objective project prioritization problem must have a
different set of projects and features (i.e., they have have different
names).

## Examples

``` r
# \dontrun{
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
 ) %>%
 add_ref_point_approach(weights = c(10, 11, 12), goals = c(3, 4, 5)) %>%
 add_default_solver()

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
#> approach:          reference point approach
#> solver:            gurobi solver

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
#> Optimize a model with 338 rows, 264 columns and 1128 nonzeros (Min)
#> Model fingerprint: 0x64ed1380
#> Model has 1 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [6e-01, 3e+01]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Presolve removed 328 rows and 240 columns
#> Presolve time: 0.00s
#> Presolved: 10 rows, 24 columns, 35 nonzeros
#> Variable types: 5 continuous, 19 integer (19 binary)
#> Found heuristic solution: objective 9.8182269
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 9.81823 
#> No other solutions better than 9.81823
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 9.818226936578e+00, best bound 9.818226936578e+00, gap 0.0000%
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
#> Optimize a model with 339 rows, 264 columns and 1129 nonzeros (Min)
#> Model fingerprint: 0x1eae4833
#> Model has 3 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e+01, 1e+01]
#>   Bounds range     [6e-01, 3e+01]
#>   RHS range        [1e+00, 2e+02]
#> 
#> User MIP start produced solution with objective 27.4033 (0.00s)
#> User MIP start produced solution with objective 27.4033 (0.00s)
#> Loaded user MIP start with objective 27.4033
#> 
#> Presolve removed 332 rows and 242 columns
#> Presolve time: 0.00s
#> Presolved: 7 rows, 22 columns, 29 nonzeros
#> Variable types: 3 continuous, 19 integer (19 binary)
#> Root relaxation presolved: 7 rows, 21 columns, 29 nonzeros
#> 
#> 
#> Root relaxation: objective 2.632626e+01, 6 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0      26.3262603   26.32626  0.00%     -    0s
#> 
#> Explored 1 nodes (6 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 26.3263 
#> No other solutions better than 26.3263
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.632626034934e+01, best bound 2.632626034934e+01, gap 0.0000%

# print solution
print(s)
#> # A tibble: 1 × 47
#>   solution status   cost  obj1  obj2  obj3 A1_action A2_action A3_action
#>      <int> <chr>   <dbl> <dbl> <dbl> <dbl> <lgl>     <lgl>     <lgl>    
#> 1        1 OPTIMAL  198. 0.420 0.430  1.75 FALSE     FALSE     FALSE    
#> # ℹ 38 more variables: A4_action <lgl>, A5_action <lgl>, A6_action <lgl>,
#> #   A7_action <lgl>, A8_action <lgl>, A9_action <lgl>, A10_action <lgl>,
#> #   A11_action <lgl>, A12_action <lgl>, A13_action <lgl>, A14_action <lgl>,
#> #   A15_action <lgl>, B1_action <lgl>, B2_action <lgl>, B3_action <lgl>,
#> #   F1_project <lgl>, F2_project <lgl>, F8_project <lgl>,
#> #   baseline_project_obj1 <lgl>, F3_project <lgl>, F4_project <lgl>,
#> #   baseline_project_obj2 <lgl>, F5_project <lgl>, F6_project <lgl>, …
# }
```
