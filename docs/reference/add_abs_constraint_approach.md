# Add an absolute constraint approach

Add an constraint approach for multi-objective optimization to a project
problem based on the required objective values.

## Usage

``` r
add_abs_constraint_approach(x, goals, verbose = TRUE)
```

## Arguments

- x:

  [`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
  object.

- goals:

  `numeric` vector containing values that denote the represent a
  threshold minimum level of achievement for each objective. If no goal
  is required for a particular objective, then a missing `NA` value can
  be specified. To generate multiple solutions based on different
  values, `goals` can be a `numeric` matrix where each row corresponds
  to a different solution and each columns corresponds to a different
  objective.

- verbose:

  `logical` should progress on generating solutions displayed? Defaults
  to `TRUE`.

## Value

A
[`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
object with the approach added to it.

## Details

Constraint-based approaches for multi-objective optimization involves
adding constraints to a problem formulation to ensure that solutions
achieve a particular level of performance each objective, whilst
maximizing performance according to a primary objective. In particular,
each objective is associated with a goal that specifies a threshold
minimum level of performance (conceptually similar to a target in the
minimum set formulation). Below we provide the mathematical details for
this approach.

To describe this approach mathematically, we will define the following
terminology. Let \\O\\ denote the set of objectives (indexed by \\o\\).
For each objective, \\G_o\\ denote the goal for each objective \\o \in
O\\, and \\V_o\\ denote the objective value for a candidate solution as
measured based on each objective \\o \in O\\. Also, let \\V_1\\ denote
the objective value for the first objective. Although we assume that all
objectives here should be maximized (for brevity), this approach is
compatible with objectives that should also be minimized. After defining
these terms, the approach is formulated with the following equation.

\$\$ \mathrm{Maximize} \space V_1 \\ \mathrm{Subject \space to} \space
\\ V_o \geq G_o \space \forall o \in O \$\$

## See also

Other approaches:
[`add_ref_point_approach()`](https://prioritizr.github.io/oppr/reference/add_ref_point_approach.md),
[`add_wtd_goal_approach()`](https://prioritizr.github.io/oppr/reference/add_wtd_goal_approach.md)

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
        budget = 1000, tree = sim_multi_tree[[1]]
      ) %>%
      add_binary_decisions(),
   obj2 =
     problem(
       sim_multi_projects[[2]], sim_multi_actions, sim_multi_features[[2]],
       "name", "success", "name", "cost", "name",
       baseline_project_name = "baseline_project_obj2"
     ) %>%
     add_max_richness_objective(budget = 1000) %>%
     add_binary_decisions(),
   obj3 =
     problem(
       sim_multi_projects[[3]], sim_multi_actions, sim_multi_features[[3]],
       "name", "success", "name", "cost", "name",
       baseline_project_name = "baseline_project_obj3"
     ) %>%
     add_max_wtd_sum_objective(budget = 1000) %>%
     add_binary_decisions()
 ) %>%
 add_abs_constraint_approach(goals = c(NA, 0.01, 0.01)) %>%
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
#> approach:          absolute constraint approach
#> solver:            gurobi solver

# solve problem
s <- solve(p)
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0
#> Set parameter ScaleFlag to value 2
#> Set parameter NumericFocus to value 1
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter PoolSolutions to value 1
#> Set parameter PoolSearchMode to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
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
#> Optimize a model with 334 rows, 260 columns and 918 nonzeros (Max)
#> Model fingerprint: 0x6755096a
#> Model has 201 linear objective coefficients
#> Variable types: 11 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [5e-02, 1e+02]
#>   Objective range  [2e-02, 5e-01]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [1e-02, 1e+03]
#> 
#> Presolve removed 244 rows and 80 columns
#> Presolve time: 0.00s
#> Presolved: 286 rows, 278 columns, 768 nonzeros
#> Variable types: 98 continuous, 180 integer (180 binary)
#> Found heuristic solution: objective 0.3873963
#> Root relaxation presolved: 286 rows, 278 columns, 768 nonzeros
#> 
#> 
#> Root relaxation: objective 8.326604e-01, 71 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.83266    0   13    0.38740    0.83266   115%     -    0s
#> H    0     0                       0.6823882    0.83266  22.0%     -    0s
#>      0     0    0.71947    0    3    0.68239    0.71947  5.43%     -    0s
#>      0     0    0.71947    0    3    0.68239    0.71947  5.43%     -    0s
#>      0     0    0.71874    0    6    0.68239    0.71874  5.33%     -    0s
#>      0     0    0.71789    0    9    0.68239    0.71789  5.20%     -    0s
#> H    0     0                       0.7096175    0.71780  1.15%     -    0s
#>      0     0    0.71780    0    9    0.70962    0.71780  1.15%     -    0s
#>      0     0    0.71780    0    9    0.70962    0.71780  1.15%     -    0s
#> H    0     0                       0.7105510    0.71775  1.01%     -    0s
#> H    0     0                       0.7113862    0.71775  0.89%     -    0s
#>      0     0    0.71775    0   11    0.71139    0.71775  0.89%     -    0s
#>      0     0    0.71775    0    9    0.71139    0.71775  0.89%     -    0s
#>      0     0    0.71771    0   11    0.71139    0.71771  0.89%     -    0s
#>      0     0    0.71766    0   11    0.71139    0.71766  0.88%     -    0s
#>      0     0    0.71766    0    9    0.71139    0.71766  0.88%     -    0s
#>      0     0    0.71765    0   15    0.71139    0.71765  0.88%     -    0s
#>      0     0    0.71756    0    9    0.71139    0.71756  0.87%     -    0s
#>      0     0    0.71745    0   11    0.71139    0.71745  0.85%     -    0s
#>      0     0    0.71745    0   11    0.71139    0.71745  0.85%     -    0s
#>      0     0    0.71745    0   13    0.71139    0.71745  0.85%     -    0s
#>      0     0    0.71745    0   15    0.71139    0.71745  0.85%     -    0s
#>      0     0    0.71743    0   11    0.71139    0.71743  0.85%     -    0s
#>      0     0    0.71739    0   11    0.71139    0.71739  0.84%     -    0s
#>      0     0    0.71737    0   13    0.71139    0.71737  0.84%     -    0s
#>      0     0    0.71736    0   15    0.71139    0.71736  0.84%     -    0s
#>      0     0    0.71736    0   11    0.71139    0.71736  0.84%     -    0s
#>      0     0    0.71735    0   17    0.71139    0.71735  0.84%     -    0s
#>      0     0    0.71735    0   17    0.71139    0.71735  0.84%     -    0s
#>      0     2    0.71709    0   17    0.71139    0.71709  0.80%     -    0s
#> 
#> Cutting planes:
#>   Gomory: 1
#>   Cover: 13
#>   Implied bound: 2
#>   MIR: 24
#> 
#> Explored 123 nodes (552 simplex iterations) in 0.07 seconds (0.05 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.711386 
#> No other solutions better than 0.711386
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 7.113861930761e-01, best bound 7.113861930761e-01, gap 0.0000%

# print solution
print(s)
#> # A tibble: 1 × 47
#>   solution status   cost  obj1  obj2  obj3 A1_action A2_action A3_action
#>      <int> <chr>   <dbl> <dbl> <dbl> <dbl> <lgl>     <lgl>     <lgl>    
#> 1        1 OPTIMAL  885. 0.670 0.430  1.62 TRUE      FALSE     TRUE     
#> # ℹ 38 more variables: A4_action <lgl>, A5_action <lgl>, A6_action <lgl>,
#> #   A7_action <lgl>, A8_action <lgl>, A9_action <lgl>, A10_action <lgl>,
#> #   A11_action <lgl>, A12_action <lgl>, A13_action <lgl>, A14_action <lgl>,
#> #   A15_action <lgl>, B1_action <lgl>, B2_action <lgl>, B3_action <lgl>,
#> #   F1_project <lgl>, F2_project <lgl>, F8_project <lgl>,
#> #   baseline_project_obj1 <lgl>, F3_project <lgl>, F4_project <lgl>,
#> #   baseline_project_obj2 <lgl>, F5_project <lgl>, F6_project <lgl>, …
# }
```
