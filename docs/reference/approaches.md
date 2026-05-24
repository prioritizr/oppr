# Multi-objective optimization approaches

Approaches specify methods for generating solutions for multi-objective
optimization problems.

## Details

The following approaches can be used to generate solutions for a
multi-objective project prioritization problem.

- [`add_abs_constraint_approach()`](https://prioritizr.github.io/oppr/reference/add_abs_constraint_approach.md):

  Add an approach to generate solutions based on constraints that
  specify the required objectives values.

- [`add_wtd_goal_approach()`](https://prioritizr.github.io/oppr/reference/add_wtd_goal_approach.md):

  Add an approach to generate solutions with the weighted goal method
  (Jones and Tamiz 2010).

- [`add_ref_point_approach()`](https://prioritizr.github.io/oppr/reference/add_ref_point_approach.md):

  Add an approach to generate solutions with the reference point method
  (Vanderpooten 1990)

## References

Jones D and Tamiz M (2010) *Goal Programming Variants*. and Management
Science, volume 141. Springer, Boston, MA.

Vanderpooten D (1990) *Multiobjective programming: Basic concepts and*
*approaches*. In: Stochastic Versus Fuzzy Approaches to Multiobjective
Mathematical Programming Under Uncertainty. Springer, Berlin.

## See also

Other overviews:
[`constraints`](https://prioritizr.github.io/oppr/reference/constraints.md),
[`objectives`](https://prioritizr.github.io/oppr/reference/objectives.md),
[`solvers`](https://prioritizr.github.io/oppr/reference/solvers.md),
[`targets`](https://prioritizr.github.io/oppr/reference/targets.md),
[`weights()`](https://prioritizr.github.io/oppr/reference/weights.md)

## Examples

``` r
# \dontrun{
# load data
data(sim_multi_projects)
data(sim_multi_features)
data(sim_multi_actions)
data(sim_multi_tree)

# build problem
p1 <-
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
 add_default_solver()

# build another problem, with the absolute constraint method
p2 <-
  p1 %>%
  add_abs_constraint_approach(
    goals = c(NA, 0.01, 0.01)
  )

# build another problem, with the weighted goal method
p3 <-
  p1 %>%
  add_wtd_goal_approach(
    weights = c(1, 0.5, 0.1),
    goals = c(1, 3, 0.2)
  )

# build another problem, with the reference point method
p4 <-
  p1 %>%
  add_ref_point_approach(
    weights = c(1, 0.5, 0.1),
    goals = c(1, 3, 0.2)
  )

# generate solutions using each approach
s <- rbind(solve(p2), solve(p3), solve(p4))
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
#> Explored 123 nodes (552 simplex iterations) in 0.08 seconds (0.05 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.711386 
#> No other solutions better than 0.711386
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 7.113861930761e-01, best bound 7.113861930761e-01, gap 0.0000%
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
#> Model fingerprint: 0x712f3a04
#> Model has 3 linear objective coefficients
#> Variable types: 14 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e-01, 1e+00]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [2e-01, 1e+03]
#> 
#> Presolve removed 243 rows and 80 columns
#> Presolve time: 0.00s
#> Presolved: 288 rows, 281 columns, 923 nonzeros
#> Variable types: 101 continuous, 180 integer (180 binary)
#> Found heuristic solution: objective 1.0409811
#> Found heuristic solution: objective 0.9421745
#> Root relaxation presolved: 288 rows, 281 columns, 923 nonzeros
#> 
#> 
#> Root relaxation: objective 5.592195e-01, 151 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.55922    0   16    0.94217    0.55922  40.6%     -    0s
#> H    0     0                       0.7459892    0.55922  25.0%     -    0s
#>      0     0    0.62914    0   19    0.74599    0.62914  15.7%     -    0s
#> H    0     0                       0.7413913    0.63606  14.2%     -    0s
#>      0     0    0.69489    0   24    0.74139    0.69489  6.27%     -    0s
#>      0     0    0.69489    0   13    0.74139    0.69489  6.27%     -    0s
#>      0     0    0.69489    0    8    0.74139    0.69489  6.27%     -    0s
#>      0     0    0.69517    0   19    0.74139    0.69517  6.23%     -    0s
#>      0     0    0.70907    0    9    0.74139    0.70907  4.36%     -    0s
#>      0     0    0.70923    0    5    0.74139    0.70923  4.34%     -    0s
#>      0     0    0.70931    0    7    0.74139    0.70931  4.33%     -    0s
#>      0     0    0.70934    0    5    0.74139    0.70934  4.32%     -    0s
#>      0     0    0.70940    0    7    0.74139    0.70940  4.32%     -    0s
#>      0     0    0.70948    0    9    0.74139    0.70948  4.30%     -    0s
#>      0     0    0.70950    0    9    0.74139    0.70950  4.30%     -    0s
#> H    0     0                       0.7331862    0.70950  3.23%     -    0s
#> H    0     0                       0.7331862    0.70950  3.23%     -    0s
#>      0     0    0.70953    0    3    0.73319    0.70953  3.23%     -    0s
#>      0     0    0.70980    0    2    0.73319    0.70980  3.19%     -    0s
#> H    0     0                       0.7169912    0.70980  1.00%     -    0s
#>      0     0    0.70989    0    5    0.71699    0.70989  0.99%     -    0s
#>      0     0    0.71582    0    4    0.71699    0.71582  0.16%     -    0s
#>      0     0    0.71589    0    7    0.71699    0.71589  0.15%     -    0s
#>      0     0    0.71646    0    7    0.71699    0.71646  0.07%     -    0s
#>      0     0    0.71657    0    9    0.71699    0.71657  0.06%     -    0s
#> 
#> Explored 1 nodes (599 simplex iterations) in 0.05 seconds (0.04 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.716991 
#> No other solutions better than 0.716991
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 7.169911970195e-01, best bound 7.169911970195e-01, gap 0.0000%
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
#> Optimize a model with 338 rows, 264 columns and 1128 nonzeros (Min)
#> Model fingerprint: 0xbf008ffe
#> Model has 1 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [1e+00, 1e+03]
#> 
#> Presolve removed 244 rows and 80 columns
#> Presolve time: 0.01s
#> Presolved: 290 rows, 282 columns, 927 nonzeros
#> Variable types: 102 continuous, 180 integer (180 binary)
#> Found heuristic solution: objective 0.8692610
#> Root relaxation presolved: 290 rows, 282 columns, 927 nonzeros
#> 
#> 
#> Root relaxation: objective 3.294653e-01, 134 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.32947    0   23    0.86926    0.32947  62.1%     -    0s
#>      0     0    0.32994    0   22    0.86926    0.32994  62.0%     -    0s
#> H    0     0                       0.6523857    0.32994  49.4%     -    0s
#> H    0     0                       0.5432595    0.32994  39.3%     -    0s
#>      0     0    0.47629    0   23    0.54326    0.47629  12.3%     -    0s
#>      0     0    0.50000    0   10    0.54326    0.50000  7.96%     -    0s
#>      0     0    0.50000    0    5    0.54326    0.50000  7.96%     -    0s
#> H    0     0                       0.5316729    0.50000  5.96%     -    0s
#> H    0     0                       0.5024569    0.50000  0.49%     -    0s
#>      0     0    0.50000    0    5    0.50246    0.50000  0.49%     -    0s
#>      0     0    0.50000    0    5    0.50246    0.50000  0.49%     -    0s
#>      0     0    0.50000    0    8    0.50246    0.50000  0.49%     -    0s
#>      0     0    0.50000    0    8    0.50246    0.50000  0.49%     -    0s
#> H    0     0                       0.5000000    0.50000  0.00%     -    0s
#>      0     0    0.50000    0    8    0.50000    0.50000  0.00%     -    0s
#> 
#> Cutting planes:
#>   Gomory: 1
#>   Cover: 4
#>   MIR: 1
#>   Relax-and-lift: 2
#> 
#> Explored 1 nodes (457 simplex iterations) in 0.03 seconds (0.03 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.5 
#> No other solutions better than 0.5
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 5.000000000000e-01, best bound 5.000000000000e-01, gap 0.0000%
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
#> Optimize a model with 339 rows, 264 columns and 1129 nonzeros (Min)
#> Model fingerprint: 0xd9e4d0a5
#> Model has 3 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [5e-02, 2e+00]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [5e-01, 1e+03]
#> 
#> User MIP start produced solution with objective 0.996685 (0.06s)
#> User MIP start produced solution with objective 0.995088 (0.07s)
#> User MIP start produced solution with objective 0.995088 (0.08s)
#> User MIP start produced solution with objective 0.99366 (0.10s)
#> User MIP start produced solution with objective 0.99366 (0.10s)
#> Loaded user MIP start with objective 0.99366
#> 
#> Presolve removed 245 rows and 80 columns
#> Presolve time: 0.01s
#> Presolved: 290 rows, 282 columns, 927 nonzeros
#> Variable types: 102 continuous, 180 integer (180 binary)
#> Root relaxation presolved: 290 rows, 282 columns, 927 nonzeros
#> 
#> 
#> Root relaxation: objective 6.587300e-01, 158 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.65873    0   16    0.99366    0.65873  33.7%     -    0s
#>      0     0    0.76266    0   23    0.99366    0.76266  23.2%     -    0s
#>      0     0    0.77119    0   27    0.99366    0.77119  22.4%     -    0s
#>      0     0    0.81948    0   16    0.99366    0.81948  17.5%     -    0s
#>      0     0    0.81948    0   14    0.99366    0.81948  17.5%     -    0s
#>      0     0    0.82366    0   15    0.99366    0.82366  17.1%     -    0s
#>      0     0    0.86987    0   12    0.99366    0.86987  12.5%     -    0s
#>      0     0    0.98028    0    6    0.99366    0.98028  1.35%     -    0s
#>      0     0    0.98238    0    5    0.99366    0.98238  1.14%     -    0s
#>      0     0    0.98238    0    3    0.99366    0.98238  1.14%     -    0s
#>      0     0    0.98238    0    1    0.99366    0.98238  1.14%     -    0s
#>      0     0    0.98238    0    5    0.99366    0.98238  1.14%     -    0s
#>      0     0    0.98325    0    7    0.99366    0.98325  1.05%     -    0s
#>      0     0    0.98345    0    7    0.99366    0.98345  1.03%     -    0s
#>      0     0    0.98355    0    7    0.99366    0.98355  1.02%     -    0s
#>      0     0    0.98358    0   11    0.99366    0.98358  1.01%     -    0s
#>      0     0    0.98373    0    7    0.99366    0.98373  1.00%     -    0s
#>      0     0    0.98373    0    7    0.99366    0.98373  1.00%     -    0s
#>      0     0    0.98380    0   11    0.99366    0.98380  0.99%     -    0s
#>      0     0    0.98389    0    9    0.99366    0.98389  0.98%     -    0s
#>      0     0    0.98389    0   11    0.99366    0.98389  0.98%     -    0s
#>      0     0    0.98395    0    7    0.99366    0.98395  0.98%     -    0s
#>      0     0    0.98399    0   13    0.99366    0.98399  0.97%     -    0s
#>      0     0    0.98399    0   11    0.99366    0.98399  0.97%     -    0s
#>      0     0    0.98400    0    9    0.99366    0.98400  0.97%     -    0s
#>      0     0    0.98400    0   17    0.99366    0.98400  0.97%     -    0s
#>      0     0    0.98402    0    7    0.99366    0.98402  0.97%     -    0s
#>      0     0    0.98414    0    9    0.99366    0.98414  0.96%     -    0s
#>      0     0    0.98414    0    9    0.99366    0.98414  0.96%     -    0s
#>      0     0    0.98415    0   11    0.99366    0.98415  0.96%     -    0s
#>      0     0    0.98424    0   11    0.99366    0.98424  0.95%     -    0s
#>      0     0    0.98427    0    9    0.99366    0.98427  0.95%     -    0s
#>      0     0    0.98428    0   13    0.99366    0.98428  0.94%     -    0s
#>      0     0    0.98431    0   13    0.99366    0.98431  0.94%     -    0s
#>      0     0    0.98431    0   13    0.99366    0.98431  0.94%     -    0s
#>      0     2    0.98435    0   13    0.99366    0.98435  0.94%     -    0s
#> 
#> Cutting planes:
#>   Cover: 8
#>   MIR: 4
#>   GUB cover: 1
#> 
#> Explored 68 nodes (745 simplex iterations) in 0.18 seconds (0.12 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.99366 
#> No other solutions better than 0.99366
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 9.936597894880e-01, best bound 9.936597894880e-01, gap 0.0000%
s$approach <- c("abs epsilon", "wtd goal", "ref point")

# print solutions
print(as.data.frame(s))
#>   solution  status     cost      obj1      obj2     obj3 A1_action A2_action
#> 1        1 OPTIMAL 884.8571 0.6701859 0.4297357 1.616604      TRUE     FALSE
#> 2        1 OPTIMAL 884.8571 0.6701859 0.4297357 1.616604      TRUE     FALSE
#> 3        1 OPTIMAL 884.8571 0.6701859 0.4297357 1.616604      TRUE     FALSE
#>   A3_action A4_action A5_action A6_action A7_action A8_action A9_action
#> 1      TRUE     FALSE      TRUE      TRUE      TRUE     FALSE     FALSE
#> 2      TRUE     FALSE      TRUE      TRUE      TRUE     FALSE     FALSE
#> 3      TRUE     FALSE      TRUE      TRUE      TRUE     FALSE     FALSE
#>   A10_action A11_action A12_action A13_action A14_action A15_action B1_action
#> 1       TRUE       TRUE       TRUE      FALSE      FALSE       TRUE      TRUE
#> 2       TRUE       TRUE       TRUE      FALSE      FALSE       TRUE      TRUE
#> 3       TRUE       TRUE       TRUE      FALSE      FALSE       TRUE      TRUE
#>   B2_action B3_action F1_project F2_project F8_project baseline_project_obj1
#> 1      TRUE      TRUE       TRUE      FALSE      FALSE                  TRUE
#> 2      TRUE      TRUE       TRUE      FALSE      FALSE                  TRUE
#> 3      TRUE      TRUE       TRUE      FALSE      FALSE                  TRUE
#>   F3_project F4_project baseline_project_obj2 F5_project F6_project F7_project
#> 1      FALSE      FALSE                  TRUE      FALSE      FALSE      FALSE
#> 2      FALSE      FALSE                  TRUE      FALSE      FALSE      FALSE
#> 3      FALSE      FALSE                  TRUE      FALSE      FALSE      FALSE
#>   F9_project F10_project baseline_project_obj3        F1        F2        F8
#> 1       TRUE       FALSE                  TRUE 0.7104848 0.3990748 0.2802669
#> 2       TRUE       FALSE                  TRUE 0.7104848 0.3990748 0.2802669
#> 3       TRUE       FALSE                  TRUE 0.7104848 0.3990748 0.2802669
#>          F3         F4       F5        F6        F7        F9        F10
#> 1 0.3814154 0.04832029 0.381568 0.1507689 0.3692636 0.6485493 0.06645404
#> 2 0.3814154 0.04832029 0.381568 0.1507689 0.3692636 0.6485493 0.06645404
#> 3 0.3814154 0.04832029 0.381568 0.1507689 0.3692636 0.6485493 0.06645404
#>      approach
#> 1 abs epsilon
#> 2    wtd goal
#> 3   ref point
# }
```
