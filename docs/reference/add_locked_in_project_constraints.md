# Add locked in project constraints

Add constraints to a project prioritization problem to ensure that
particular projects are selected for funding by the solution. For
example, it may be desirable to lock in projects for conserving
culturally or taxonomically important species.

## Usage

``` r
add_locked_in_project_constraints(x, locked_in)

# S4 method for class 'ProjectProblem,numeric'
add_locked_in_project_constraints(x, locked_in)

# S4 method for class 'ProjectProblem,logical'
add_locked_in_project_constraints(x, locked_in)

# S4 method for class 'ProjectProblem,character'
add_locked_in_project_constraints(x, locked_in)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  object.

- locked_in:

  Object that determines which planning units that should be locked in.
  See the Details section for more information.

## Value

A [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
object with the constraints added to it.

## Details

The locked projects can be specified in several different ways:

- `integer` vector:

  Each values specifies the index for a project that should be locked
  when generating solutions (i.e., row numbers of the projects in the
  argument to `projects` in
  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)).

- `logical` vector:

  Each value (i.e., `TRUE` and/or `FALSE`) indicates if a project should
  be locked (or not) when generating the solution. These `logical`
  values should correspond to each row in the argument to `projects` in
  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md).

- `character` value:

  The value specifies the name of a column in the project data (i.e.,
  argument to `projects` in
  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)).
  The column must have `logical` (i.e., `TRUE` and/or `FALSE`) values,
  and these values are used to indicate which projects are locked (or
  not).

## See also

Other constraints:
[`add_locked_in_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_in_action_constraints.md),
[`add_locked_out_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_out_action_constraints.md),
[`add_locked_out_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_out_project_constraints.md),
[`add_manual_locked_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_manual_locked_action_constraints.md),
[`add_manual_locked_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_manual_locked_project_constraints.md)

## Examples

``` r
# \dontrun{
# load data
data(sim_projects, sim_features, sim_actions)

# add column to the project data to indicate which projects should be
# locked. in particular, this column will lock the first project
sim_projects$locked_in <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))

# print project data
print(sim_projects)
#> # A tibble: 6 × 14
#>   name           success     F1     F2      F3     F4     F5 F1_action F2_action
#>   <chr>            <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl> <lgl>     <lgl>    
#> 1 F1_project       0.919  0.791 NA     NA      NA     NA     TRUE      FALSE    
#> 2 F2_project       0.923 NA      0.888 NA      NA     NA     FALSE     TRUE     
#> 3 F3_project       0.829 NA     NA      0.502  NA     NA     FALSE     FALSE    
#> 4 F4_project       0.848 NA     NA     NA       0.690 NA     FALSE     FALSE    
#> 5 F5_project       0.814 NA     NA     NA      NA      0.617 FALSE     FALSE    
#> 6 baseline_proj…   1      0.298  0.250  0.0865  0.249  0.182 FALSE     FALSE    
#> # ℹ 5 more variables: F3_action <lgl>, F4_action <lgl>, F5_action <lgl>,
#> #   baseline_action <lgl>, locked_in <lgl>

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

# build another problem, and lock in the 3rd project using numeric inputs
p2 <- p1 %>% add_locked_in_project_constraints(c(3))

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
#> constraints:     locked in projects
#> decisions:       binary decision
#> solver:          none specified

# build another problem, and lock in the projects using logical inputs from
# the sim_projects stable
p3 <- p1 %>% add_locked_in_project_constraints(sim_projects$locked_in)

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
#> constraints:     locked in projects
#> decisions:       binary decision
#> solver:          none specified

# build another problem, and lock in the projects using the column name
# "locked_in" in the sim_projects table
p4 <- p1 %>% add_locked_in_project_constraints("locked_in")

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
#> constraints:     locked in projects
#> decisions:       binary decision
#> solver:          none specified

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
#> Model fingerprint: 0x8c7e1704
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Found heuristic solution: objective 1.0652051
#> Presolve removed 27 rows and 26 columns
#> Presolve time: 0.00s
#> Presolved: 0 rows, 1 columns, 0 nonzeros
#> Variable types: 0 continuous, 1 integer (1 binary)
#> Root relaxation presolve: All rows and columns removed
#> 
#> Root relaxation: objective 1.445609e+00, 0 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       1.4456093    1.44561  0.00%     -    0s
#> 
#> Explored 1 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
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
#> Model fingerprint: 0x5b82c63d
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Found heuristic solution: objective 1.5754408
#> Presolve removed 27 rows and 26 columns
#> Presolve time: 0.00s
#> Presolved: 0 rows, 1 columns, 0 nonzeros
#> Variable types: 0 continuous, 1 integer (1 binary)
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
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
#> Model fingerprint: 0x5b82c63d
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Found heuristic solution: objective 1.5754408
#> Presolve removed 27 rows and 26 columns
#> Presolve time: 0.00s
#> Presolved: 0 rows, 1 columns, 0 nonzeros
#> Variable types: 0 continuous, 1 integer (1 binary)
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 1.57544 
#> No other solutions better than 1.57544
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 1.575440809243e+00, best bound 1.575440809243e+00, gap 0.0000%

# print the projects selected for funding in each of the solutions
print(s1[, sim_projects$name])
#> # A tibble: 1 × 6
#>   F1_project F2_project F3_project F4_project F5_project baseline_project
#>   <lgl>      <lgl>      <lgl>      <lgl>      <lgl>      <lgl>           
#> 1 FALSE      TRUE       FALSE      FALSE      FALSE      TRUE            
print(s2[, sim_projects$name])
#> # A tibble: 1 × 6
#>   F1_project F2_project F3_project F4_project F5_project baseline_project
#>   <lgl>      <lgl>      <lgl>      <lgl>      <lgl>      <lgl>           
#> 1 FALSE      FALSE      TRUE       FALSE      FALSE      TRUE            
print(s3[, sim_projects$name])
#> # A tibble: 1 × 6
#>   F1_project F2_project F3_project F4_project F5_project baseline_project
#>   <lgl>      <lgl>      <lgl>      <lgl>      <lgl>      <lgl>           
#> 1 TRUE       FALSE      FALSE      FALSE      FALSE      TRUE            
print(s4[, sim_projects$name])
#> # A tibble: 1 × 6
#>   F1_project F2_project F3_project F4_project F5_project baseline_project
#>   <lgl>      <lgl>      <lgl>      <lgl>      <lgl>      <lgl>           
#> 1 TRUE       FALSE      FALSE      FALSE      FALSE      TRUE            
# }
```
