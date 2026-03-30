# Project prioritization problem constraints

A constraint can be added to a project prioritization
[`problem()`](https://prioritizr.github.io/oppr/reference/problem.md) to
ensure that solutions exhibit a specific characteristic.

## Details

The following constraints can be added to a project prioritization
[`problem()`](https://prioritizr.github.io/oppr/reference/problem.md):

- [`add_locked_in_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_in_action_constraints.md):

  Add constraints to ensure that particular actions are selected for
  funding.

- [`add_locked_out_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_out_action_constraints.md):

  Add constraints to ensure that particular actions are not selected for
  funding.

- [`add_manual_locked_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_manual_locked_action_constraints.md):

  Add constraints to ensure that particular actions are selected, or
  not, for funding.

- [`add_locked_in_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_in_project_constraints.md):

  Add constraints to ensure that particular projects are selected for
  funding.

- [`add_locked_out_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_out_project_constraints.md):

  Add constraints to ensure that particular projects are not selected
  for funding.

- [`add_manual_locked_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_manual_locked_project_constraints.md):

  Add constraints to ensure that particular projects are selected, or
  not, for funding.

## See also

Other overviews:
[`approaches`](https://prioritizr.github.io/oppr/reference/approaches.md),
[`objectives`](https://prioritizr.github.io/oppr/reference/objectives.md),
[`solvers`](https://prioritizr.github.io/oppr/reference/solvers.md),
[`targets`](https://prioritizr.github.io/oppr/reference/targets.md),
[`weights()`](https://prioritizr.github.io/oppr/reference/weights.md)

## Examples

``` r
# load data
data(sim_projects, sim_features, sim_actions)

# build problem with maximum weighted sum objective and $150 budget
p1 <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_max_wtd_sum_objective(budget = 150) %>%
  add_binary_decisions()

# print problem
print(p1)
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

# build another problem, and lock in the third action
p2 <- p1 %>% add_locked_in_action_constraints(c(3))

# print problem
print(p2)
#> Project Prioritization Problem
#> actions:         F1_action, F2_action, F3_action, ... (6 actions)
#> projects:        F1_project, F2_project, F3_project, ... (6 projects)
#> features:        F1, F2, F3, ... (5 features)
#> action costs:    continuous values (between 0 and 103.226)
#> project success: proportion values (between 0.814 and 1)
#> objective:       maximum weighted sum objective
#> targets:         none specified
#> weights:         none specified
#> constraints:     locked in actions
#> decisions:       binary decision
#> solver:          none specified

# build another problem, and lock out the second action
p3 <- p1 %>% add_locked_out_action_constraints(c(2))

# print problem
print(p3)
#> Project Prioritization Problem
#> actions:         F1_action, F2_action, F3_action, ... (6 actions)
#> projects:        F1_project, F2_project, F3_project, ... (6 projects)
#> features:        F1, F2, F3, ... (5 features)
#> action costs:    continuous values (between 0 and 103.226)
#> project success: proportion values (between 0.814 and 1)
#> objective:       maximum weighted sum objective
#> targets:         none specified
#> weights:         none specified
#> constraints:     manually locked actions
#> decisions:       binary decision
#> solver:          none specified

# build another problem, and lock in the first project
p4 <- p1 %>% add_locked_out_project_constraints(c(1))

# print problem
print(p4)
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

# build another problem, and lock out the second project
p5 <- p1 %>% add_locked_out_project_constraints(c(2))

# print problem
print(p5)
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

# \dontrun{
# solve problems
s1 <- solve(p1)
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
#> Model fingerprint: 0x561c9c42
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Found heuristic solution: objective 1.4456093
#> Presolve removed 16 rows and 12 columns
#> Presolve time: 0.00s
#> Presolved: 11 rows, 15 columns, 25 nonzeros
#> Variable types: 0 continuous, 15 integer (15 binary)
#> Root relaxation presolved: 11 rows, 15 columns, 25 nonzeros
#> 
#> 
#> Root relaxation: objective 1.680145e+00, 11 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       1.6801450    1.68015  0.00%     -    0s
#> 
#> Explored 1 nodes (11 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 1.68015 
#> No other solutions better than 1.68015
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 1.680145013696e+00, best bound 1.680145013696e+00, gap 0.0000%
s2 <- solve(p2)
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
#> Model fingerprint: 0x55efe312
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Found heuristic solution: objective 1.4456093
#> Presolve removed 26 rows and 25 columns
#> Presolve time: 0.00s
#> Presolved: 1 rows, 2 columns, 2 nonzeros
#> Variable types: 0 continuous, 2 integer (2 binary)
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 1.44561 
#> No other solutions better than 1.44561
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 1.445609277954e+00, best bound 1.445609277954e+00, gap 0.0000%
s3 <- solve(p3)
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
#> Model fingerprint: 0xfd5c060b
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Found heuristic solution: objective 1.4456093
#> Presolve removed 18 rows and 15 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 12 columns, 20 nonzeros
#> Variable types: 0 continuous, 12 integer (12 binary)
#> Root relaxation presolved: 9 rows, 12 columns, 20 nonzeros
#> 
#> 
#> Root relaxation: objective 1.575441e+00, 9 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       1.5754408    1.57544  0.00%     -    0s
#> 
#> Explored 1 nodes (9 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 1.57544 
#> No other solutions better than 1.57544
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 1.575440809243e+00, best bound 1.575440809243e+00, gap 0.0000%
s4 <- solve(p4)
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
#> Model fingerprint: 0x3a769447
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Found heuristic solution: objective 1.4456093
#> Presolve removed 18 rows and 14 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 13 columns, 21 nonzeros
#> Variable types: 0 continuous, 13 integer (13 binary)
#> Root relaxation presolved: 9 rows, 13 columns, 21 nonzeros
#> 
#> 
#> Root relaxation: objective 1.680145e+00, 9 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       1.6801450    1.68015  0.00%     -    0s
#> 
#> Explored 1 nodes (9 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 1.68015 
#> No other solutions better than 1.68015
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 1.680145013696e+00, best bound 1.680145013696e+00, gap 0.0000%
s5 <- solve(p5)
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
#> Model fingerprint: 0xfb20e0e5
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Found heuristic solution: objective 1.4456093
#> Presolve removed 18 rows and 14 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 13 columns, 21 nonzeros
#> Variable types: 0 continuous, 13 integer (13 binary)
#> Root relaxation presolved: 9 rows, 13 columns, 21 nonzeros
#> 
#> 
#> Root relaxation: objective 1.575441e+00, 9 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       1.5754408    1.57544  0.00%     -    0s
#> 
#> Explored 1 nodes (9 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 1.57544 
#> No other solutions better than 1.57544
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 1.575440809243e+00, best bound 1.575440809243e+00, gap 0.0000%

# print the projects selected for funding in each of the solutions
print(s1[, sim_actions$name])
#> # A tibble: 1 × 6
#>   F1_action F2_action F3_action F4_action F5_action baseline_action
#>   <lgl>     <lgl>     <lgl>     <lgl>     <lgl>     <lgl>          
#> 1 FALSE     TRUE      FALSE     FALSE     FALSE     TRUE           
print(s2[, sim_actions$name])
#> # A tibble: 1 × 6
#>   F1_action F2_action F3_action F4_action F5_action baseline_action
#>   <lgl>     <lgl>     <lgl>     <lgl>     <lgl>     <lgl>          
#> 1 FALSE     FALSE     TRUE      FALSE     FALSE     TRUE           
print(s3[, sim_actions$name])
#> # A tibble: 1 × 6
#>   F1_action F2_action F3_action F4_action F5_action baseline_action
#>   <lgl>     <lgl>     <lgl>     <lgl>     <lgl>     <lgl>          
#> 1 TRUE      FALSE     FALSE     FALSE     FALSE     TRUE           
print(s4[, sim_actions$name])
#> # A tibble: 1 × 6
#>   F1_action F2_action F3_action F4_action F5_action baseline_action
#>   <lgl>     <lgl>     <lgl>     <lgl>     <lgl>     <lgl>          
#> 1 FALSE     TRUE      FALSE     FALSE     FALSE     TRUE           
print(s5[, sim_actions$name])
#> # A tibble: 1 × 6
#>   F1_action F2_action F3_action F4_action F5_action baseline_action
#>   <lgl>     <lgl>     <lgl>     <lgl>     <lgl>     <lgl>          
#> 1 TRUE      FALSE     FALSE     FALSE     FALSE     TRUE           
# }
```
