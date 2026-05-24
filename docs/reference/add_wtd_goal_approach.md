# Add a weighted goal achievement approach

Add a weighted goal achievement approach for multi-objective
optimization to a project problem (Jones and Tamiz 2010).

## Usage

``` r
add_wtd_goal_approach(x, weights, goals, verbose = TRUE)
```

## Arguments

- x:

  [`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
  object.

- weights:

  `numeric` vector containing the weights for each objective. To
  generate multiple solutions based on different values, `weights` can
  be a `numeric` matrix where each row corresponds to a different
  solution and each columns corresponds to a different objective.

- goals:

  `numeric` vector containing values that denote the reference points.
  These points represent aspirational goals for each objective. To
  generate multiple solutions based on different values, `goals` can be
  a `numeric` matrix where each row corresponds to a different solution
  and each columns corresponds to a different objective. Note that all
  values must be greater than zero.

- verbose:

  `logical` should progress on generating solutions displayed? Defaults
  to `TRUE`.

## Value

A
[`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
object with the approach added to it.

## Details

The weighted goal achievement approach for multi-objective optimization
involves creating a new objective that is calculated based on multiple
objectives. In particular, the new objective uses weights to specify the
relative importance of each individual objective, and goals to specify a
threshold minimum level of performance for each objective (conceptually
similar to target thresholds used in conservation planning). It then
calculates the new objectives based on the weighted sum of the
percentage of each goal that is achieved for each objective.

To describe this approach mathematically, we will define the following
terminology. Let \\O\\ denote the set of objectives (indexed by \\o\\).
For each objective, let \\W_o\\ denote the weight for each objective \\o
\in O\\, \\G_o\\ denote the goal for each objective \\o \in O\\, and
\\V_o\\ denote the objective value for a candidate solution as measured
based on each objective \\o \in O\\. After defining these terms, the
approach is formulated with the following equation.

\$\$ \mathrm{Minimize} \space \sum\_{o = 0}^{O} W_o \times
\frac{V_o}{G_o} \$\$

## References

Jones D and Tamiz M (2010) *Goal Programming Variants*. and Management
Science, volume 141. Springer, Boston, MA.

## See also

Other approaches:
[`add_abs_constraint_approach()`](https://prioritizr.github.io/oppr/reference/add_abs_constraint_approach.md),
[`add_ref_point_approach()`](https://prioritizr.github.io/oppr/reference/add_ref_point_approach.md)

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
 add_wtd_goal_approach(weights = c(10, 11, 12), goals = c(3, 4, 5)) %>%
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
#> approach:          weighted goal approach
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
#> Optimize a model with 335 rows, 263 columns and 1122 nonzeros (Min)
#> Model fingerprint: 0x9528ff18
#> Model has 3 linear objective coefficients
#> Variable types: 14 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e+01, 1e+01]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [1e+00, 1e+03]
#> 
#> Presolve removed 242 rows and 80 columns
#> Presolve time: 0.00s
#> Presolved: 289 rows, 281 columns, 928 nonzeros
#> Variable types: 101 continuous, 180 integer (180 binary)
#> Found heuristic solution: objective 25.5700391
#> Found heuristic solution: objective 24.6994718
#> Root relaxation presolved: 289 rows, 281 columns, 928 nonzeros
#> 
#> 
#> Root relaxation: objective 2.329846e+01, 132 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0   23.29846    0   19   24.69947   23.29846  5.67%     -    0s
#>      0     0   23.36824    0   19   24.69947   23.36824  5.39%     -    0s
#>      0     0   23.92674    0   16   24.69947   23.92674  3.13%     -    0s
#>      0     0   23.98388    0   19   24.69947   23.98388  2.90%     -    0s
#>      0     0   24.09664    0   20   24.69947   24.09664  2.44%     -    0s
#>      0     0   24.34277    0   27   24.69947   24.34277  1.44%     -    0s
#>      0     0   24.34277    0   31   24.69947   24.34277  1.44%     -    0s
#>      0     0   24.34277    0   31   24.69947   24.34277  1.44%     -    0s
#>      0     0   24.34277    0   29   24.69947   24.34277  1.44%     -    0s
#>      0     0   24.34277    0   16   24.69947   24.34277  1.44%     -    0s
#>      0     0   24.34277    0   22   24.69947   24.34277  1.44%     -    0s
#>      0     0   24.34277    0   22   24.69947   24.34277  1.44%     -    0s
#>      0     0   24.34277    0   25   24.69947   24.34277  1.44%     -    0s
#>      0     0   24.36402    0   21   24.69947   24.36402  1.36%     -    0s
#>      0     0   24.40847    0   23   24.69947   24.40847  1.18%     -    0s
#>      0     0   24.42298    0   24   24.69947   24.42298  1.12%     -    0s
#>      0     0   24.42298    0   24   24.69947   24.42298  1.12%     -    0s
#>      0     0   24.53181    0   19   24.69947   24.53181  0.68%     -    0s
#>      0     0   24.54063    0   25   24.69947   24.54063  0.64%     -    0s
#>      0     0   24.54063    0   27   24.69947   24.54063  0.64%     -    0s
#>      0     0   24.54080    0   25   24.69947   24.54080  0.64%     -    0s
#>      0     0   24.54086    0   23   24.69947   24.54086  0.64%     -    0s
#>      0     0   24.54093    0   25   24.69947   24.54093  0.64%     -    0s
#>      0     0   24.54093    0   26   24.69947   24.54093  0.64%     -    0s
#>      0     0   24.54101    0   24   24.69947   24.54101  0.64%     -    0s
#>      0     0   24.54101    0   26   24.69947   24.54101  0.64%     -    0s
#>      0     0   24.54102    0   26   24.69947   24.54102  0.64%     -    0s
#>      0     0   24.54102    0   26   24.69947   24.54102  0.64%     -    0s
#>      0     2   24.58380    0   26   24.69947   24.58380  0.47%     -    0s
#> 
#> Cutting planes:
#>   Cover: 9
#>   MIR: 2
#>   RLT: 2
#>   Relax-and-lift: 1
#> 
#> Explored 29 nodes (696 simplex iterations) in 0.07 seconds (0.06 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 24.6995 
#> No other solutions better than 24.6995
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.469947178589e+01, best bound 2.469947178589e+01, gap 0.0000%

# print solution
print(s)
#> # A tibble: 1 × 47
#>   solution status   cost  obj1  obj2  obj3 A1_action A2_action A3_action
#>      <int> <chr>   <dbl> <dbl> <dbl> <dbl> <lgl>     <lgl>     <lgl>    
#> 1        1 OPTIMAL  971. 0.420  1.02  1.75 TRUE      FALSE     TRUE     
#> # ℹ 38 more variables: A4_action <lgl>, A5_action <lgl>, A6_action <lgl>,
#> #   A7_action <lgl>, A8_action <lgl>, A9_action <lgl>, A10_action <lgl>,
#> #   A11_action <lgl>, A12_action <lgl>, A13_action <lgl>, A14_action <lgl>,
#> #   A15_action <lgl>, B1_action <lgl>, B2_action <lgl>, B3_action <lgl>,
#> #   F1_project <lgl>, F2_project <lgl>, F8_project <lgl>,
#> #   baseline_project_obj1 <lgl>, F3_project <lgl>, F4_project <lgl>,
#> #   baseline_project_obj2 <lgl>, F5_project <lgl>, F6_project <lgl>, …
# }
```
