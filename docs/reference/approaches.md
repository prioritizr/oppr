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

- [`add_chebyshev_goal_approach()`](https://prioritizr.github.io/oppr/reference/add_chebyshev_goal_approach.md):

  Add an approach to generate solutions with the Chebyshev goal method.
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

# build another problem, with the Chebyshev goal method
p4 <-
  p1 %>%
  add_chebyshev_goal_approach(
    weights = c(1, 0.5, 0.1),
    goals = c(1, 3, 0.2)
  )

# build another problem, with the reference point method
p5 <-
  p1 %>%
  add_ref_point_approach(
    weights = c(1, 0.5, 0.1),
    goals = c(1, 3, 0.2)
  )

# generate solutions using each approach
s <- rbind(solve(p2), solve(p3), solve(p4), solve(p5))
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
#> Optimize a model with 334 rows, 260 columns and 918 nonzeros (Max)
#> Model fingerprint: 0xbfe6d015
#> Model has 201 linear objective coefficients
#> Variable types: 11 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [5e-02, 1e+02]
#>   Objective range  [2e-02, 5e-01]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [1e-02, 1e+03]
#> Presolve removed 243 rows and 79 columns
#> Presolve time: 0.00s
#> Presolved: 289 rows, 280 columns, 825 nonzeros
#> Variable types: 99 continuous, 181 integer (181 binary)
#> Found heuristic solution: objective 0.3884525
#> Root relaxation presolved: 289 rows, 280 columns, 825 nonzeros
#> 
#> 
#> Root relaxation: objective 8.350724e-01, 72 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.83507    0   13    0.38845    0.83507   115%     -    0s
#> H    0     0                       0.6812445    0.83507  22.6%     -    0s
#>      0     0    0.82040    0   14    0.68124    0.82040  20.4%     -    0s
#>      0     0    0.82040    0   12    0.68124    0.82040  20.4%     -    0s
#>      0     0    0.82040    0   13    0.68124    0.82040  20.4%     -    0s
#>      0     0    0.79635    0   21    0.68124    0.79635  16.9%     -    0s
#>      0     0    0.72105    0    7    0.68124    0.72105  5.84%     -    0s
#> H    0     0                       0.6859629    0.72105  5.12%     -    0s
#> H    0     0                       0.7083193    0.72105  1.80%     -    0s
#>      0     0    0.72100    0    7    0.70832    0.72100  1.79%     -    0s
#>      0     0    0.72100    0    3    0.70832    0.72100  1.79%     -    0s
#>      0     0    0.72100    0    5    0.70832    0.72100  1.79%     -    0s
#>      0     0    0.72100    0    7    0.70832    0.72100  1.79%     -    0s
#>      0     0    0.72020    0    7    0.70832    0.72020  1.68%     -    0s
#> H    0     0                       0.7103971    0.72016  1.37%     -    0s
#>      0     0    0.72014    0    7    0.71040    0.72014  1.37%     -    0s
#>      0     2    0.72002    0    7    0.71040    0.72002  1.36%     -    0s
#> H  128    26                       0.7112894    0.71848  1.01%   2.5    0s
#> H  147    29                       0.7120817    0.71836  0.88%   2.3    0s
#> 
#> Cutting planes:
#>   Cover: 18
#>   MIR: 5
#> 
#> Explored 197 nodes (628 simplex iterations) in 0.06 seconds (0.04 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.712082 
#> No other solutions better than 0.712082
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 7.120816581805e-01, best bound 7.120816581805e-01, gap 0.0000%
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
#> Optimize a model with 335 rows, 263 columns and 1122 nonzeros (Min)
#> Model fingerprint: 0x0aa66dd2
#> Model has 3 linear objective coefficients
#> Variable types: 14 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e-01, 1e+00]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [2e-01, 1e+03]
#> Presolve removed 242 rows and 79 columns
#> Presolve time: 0.00s
#> Presolved: 291 rows, 283 columns, 981 nonzeros
#> Variable types: 102 continuous, 181 integer (181 binary)
#> Found heuristic solution: objective 1.0399249
#> Found heuristic solution: objective 0.9411183
#> Root relaxation presolved: 291 rows, 283 columns, 981 nonzeros
#> 
#> 
#> Root relaxation: objective 5.569304e-01, 127 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.55693    0   16    0.94112    0.55693  40.8%     -    0s
#> H    0     0                       0.7471329    0.55693  25.5%     -    0s
#>      0     0    0.59887    0   17    0.74713    0.59887  19.8%     -    0s
#>      0     0    0.61642    0   14    0.74713    0.61642  17.5%     -    0s
#> H    0     0                       0.7424145    0.61642  17.0%     -    0s
#>      0     0    0.62798    0   25    0.74241    0.62798  15.4%     -    0s
#>      0     0    0.62798    0   12    0.74241    0.62798  15.4%     -    0s
#>      0     0    0.63198    0   18    0.74241    0.63198  14.9%     -    0s
#>      0     0    0.70725    0    3    0.74241    0.70725  4.74%     -    0s
#>      0     0    0.70732    0    7    0.74241    0.70732  4.73%     -    0s
#>      0     0    0.70738    0    7    0.74241    0.70738  4.72%     -    0s
#>      0     0    0.70747    0   11    0.74241    0.70747  4.71%     -    0s
#>      0     0    0.70747    0   11    0.74241    0.70747  4.71%     -    0s
#>      0     0    0.70747    0   11    0.74241    0.70747  4.71%     -    0s
#> H    0     0                       0.7380263    0.70751  4.13%     -    0s
#>      0     0    0.70814    0    3    0.73803    0.70814  4.05%     -    0s
#>      0     0    0.70814    0    5    0.73803    0.70814  4.05%     -    0s
#>      0     0    0.70814    0    7    0.73803    0.70814  4.05%     -    0s
#>      0     0    0.70818    0    7    0.73803    0.70818  4.04%     -    0s
#> H    0     0                       0.7179803    0.70822  1.36%     -    0s
#>      0     0    0.70823    0    7    0.71798    0.70823  1.36%     -    0s
#>      0     0    0.70823    0    7    0.71798    0.70823  1.36%     -    0s
#>      0     2    0.70829    0    7    0.71798    0.70829  1.35%     -    0s
#> H   29    19                       0.7162957    0.70855  1.08%   2.6    0s
#> 
#> Cutting planes:
#>   Cover: 19
#>   MIR: 4
#> 
#> Explored 179 nodes (664 simplex iterations) in 0.08 seconds (0.05 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.716296 
#> No other solutions better than 0.716296
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 7.162957319151e-01, best bound 7.162957319151e-01, gap 0.0000%
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
#> Optimize a model with 338 rows, 264 columns and 1128 nonzeros (Min)
#> Model fingerprint: 0x0d6d14cc
#> Model has 1 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [2e-01, 1e+03]
#> Presolve removed 243 rows and 79 columns
#> Presolve time: 0.01s
#> Presolved: 293 rows, 284 columns, 985 nonzeros
#> Variable types: 103 continuous, 181 integer (181 binary)
#> Found heuristic solution: objective 0.6115475
#> Root relaxation presolved: 293 rows, 284 columns, 985 nonzeros
#> 
#> 
#> Root relaxation: objective 3.403610e-01, 142 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.34036    0   24    0.61155    0.34036  44.3%     -    0s
#> H    0     0                       0.4283774    0.34036  20.5%     -    0s
#>      0     0    0.34861    0   24    0.42838    0.34861  18.6%     -    0s
#>      0     0 infeasible    0         0.42838    0.42838  0.00%     -    0s
#> 
#> Cutting planes:
#>   Cover: 1
#>   MIR: 1
#>   Flow cover: 1
#>   RLT: 1
#> 
#> Explored 1 nodes (255 simplex iterations) in 0.01 seconds (0.02 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.428377 
#> No other solutions better than 0.428377
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 4.283773900957e-01, best bound 4.283773900957e-01, gap 0.0000%
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
#> Optimize a model with 338 rows, 264 columns and 1128 nonzeros (Min)
#> Model fingerprint: 0x0d6d14cc
#> Model has 1 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [2e-01, 1e+03]
#> Presolve removed 243 rows and 79 columns
#> Presolve time: 0.00s
#> Presolved: 293 rows, 284 columns, 985 nonzeros
#> Variable types: 103 continuous, 181 integer (181 binary)
#> Found heuristic solution: objective 0.6115475
#> Root relaxation presolved: 293 rows, 284 columns, 985 nonzeros
#> 
#> 
#> Root relaxation: objective 3.403610e-01, 142 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.34036    0   24    0.61155    0.34036  44.3%     -    0s
#> H    0     0                       0.4283774    0.34036  20.5%     -    0s
#>      0     0    0.34861    0   24    0.42838    0.34861  18.6%     -    0s
#>      0     0 infeasible    0         0.42838    0.42838  0.00%     -    0s
#> 
#> Cutting planes:
#>   Cover: 1
#>   MIR: 1
#>   Flow cover: 1
#>   RLT: 1
#> 
#> Explored 1 nodes (255 simplex iterations) in 0.01 seconds (0.02 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.428377 
#> No other solutions better than 0.428377
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 4.283773900957e-01, best bound 4.283773900957e-01, gap 0.0000%
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
#> Optimize a model with 339 rows, 264 columns and 1129 nonzeros (Min)
#> Model fingerprint: 0x9b813812
#> Model has 3 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e-01, 1e+00]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [2e-01, 1e+03]
#> 
#> Loaded user MIP start with objective 0.856755
#> 
#> Presolve removed 244 rows and 79 columns
#> Presolve time: 0.01s
#> Presolved: 293 rows, 284 columns, 985 nonzeros
#> Variable types: 103 continuous, 181 integer (181 binary)
#> Root relaxation presolved: 293 rows, 284 columns, 985 nonzeros
#> 
#> 
#> Root relaxation: objective 5.569304e-01, 133 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.55693    0   16    0.85675    0.55693  35.0%     -    0s
#> H    0     0                       0.7471329    0.55693  25.5%     -    0s
#>      0     0    0.59590    0   16    0.74713    0.59590  20.2%     -    0s
#> H    0     0                       0.7424145    0.61703  16.9%     -    0s
#>      0     0    0.61703    0   17    0.74241    0.61703  16.9%     -    0s
#>      0     0    0.61703    0   15    0.74241    0.61703  16.9%     -    0s
#>      0     0    0.61703    0   13    0.74241    0.61703  16.9%     -    0s
#>      0     0    0.63133    0   16    0.74241    0.63133  15.0%     -    0s
#>      0     0    0.63133    0   22    0.74241    0.63133  15.0%     -    0s
#> H    0     0                       0.7380263    0.63258  14.3%     -    0s
#>      0     0    0.70731    0   20    0.73803    0.70731  4.16%     -    0s
#>      0     0    0.70752    0    6    0.73803    0.70752  4.13%     -    0s
#>      0     0    0.70752    0    6    0.73803    0.70752  4.13%     -    0s
#> H    0     0                       0.7170880    0.70758  1.33%     -    0s
#>      0     0    0.70758    0    3    0.71709    0.70758  1.33%     -    0s
#>      0     0    0.70758    0    5    0.71709    0.70758  1.33%     -    0s
#>      0     0    0.70762    0    7    0.71709    0.70762  1.32%     -    0s
#>      0     0    0.70836    0    6    0.71709    0.70836  1.22%     -    0s
#>      0     0    0.70841    0    9    0.71709    0.70841  1.21%     -    0s
#>      0     0    0.70841    0    9    0.71709    0.70841  1.21%     -    0s
#> H    0     0                       0.7162957    0.70842  1.10%     -    0s
#>      0     2    0.70842    0    9    0.71630    0.70842  1.10%     -    0s
#> 
#> Cutting planes:
#>   Cover: 11
#>   MIR: 1
#>   Flow cover: 1
#>   RLT: 1
#> 
#> Explored 210 nodes (815 simplex iterations) in 0.07 seconds (0.05 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.716296 
#> No other solutions better than 0.716296
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 7.162957319151e-01, best bound 7.162957319151e-01, gap 0.0000%
s$approach <- c("abs epsilon", "wtd goal", "Chebyshev goal", "ref point")

# print solutions
print(as.data.frame(s))
#>   solution  status     cost      obj1      obj2     obj3 A1_action A2_action
#> 1        1 OPTIMAL 884.8571 0.6701859 0.4297357 1.616604      TRUE     FALSE
#> 2        1 OPTIMAL 884.8571 0.6701859 0.4297357 1.616604      TRUE     FALSE
#> 3        1 OPTIMAL 971.6253 0.5989621 0.4297357 1.748802      TRUE      TRUE
#> 4        1 OPTIMAL 884.8571 0.6701859 0.4297357 1.616604      TRUE     FALSE
#>   A3_action A4_action A5_action A6_action A7_action A8_action A9_action
#> 1      TRUE     FALSE      TRUE      TRUE      TRUE     FALSE     FALSE
#> 2      TRUE     FALSE      TRUE      TRUE      TRUE     FALSE     FALSE
#> 3      TRUE      TRUE     FALSE     FALSE     FALSE      TRUE     FALSE
#> 4      TRUE     FALSE      TRUE      TRUE      TRUE     FALSE     FALSE
#>   A10_action A11_action A12_action A13_action A14_action A15_action B1_action
#> 1       TRUE       TRUE       TRUE      FALSE      FALSE       TRUE      TRUE
#> 2       TRUE       TRUE       TRUE      FALSE      FALSE       TRUE      TRUE
#> 3       TRUE       TRUE      FALSE       TRUE       TRUE       TRUE      TRUE
#> 4       TRUE       TRUE       TRUE      FALSE      FALSE       TRUE      TRUE
#>   B2_action B3_action F1_project F2_project F8_project baseline_project_obj1
#> 1      TRUE      TRUE       TRUE      FALSE      FALSE                  TRUE
#> 2      TRUE      TRUE       TRUE      FALSE      FALSE                  TRUE
#> 3      TRUE      TRUE      FALSE       TRUE      FALSE                  TRUE
#> 4      TRUE      TRUE       TRUE      FALSE      FALSE                  TRUE
#>   F3_project F4_project baseline_project_obj2 F5_project F6_project F7_project
#> 1      FALSE      FALSE                  TRUE      FALSE      FALSE      FALSE
#> 2      FALSE      FALSE                  TRUE      FALSE      FALSE      FALSE
#> 3      FALSE      FALSE                  TRUE      FALSE       TRUE      FALSE
#> 4      FALSE      FALSE                  TRUE      FALSE      FALSE      FALSE
#>   F9_project F10_project baseline_project_obj3        F1        F2        F8
#> 1       TRUE       FALSE                  TRUE 0.7104848 0.3990748 0.2802669
#> 2       TRUE       FALSE                  TRUE 0.7104848 0.3990748 0.2802669
#> 3      FALSE       FALSE                  TRUE 0.1391247 0.7582101 0.2802669
#> 4       TRUE       FALSE                  TRUE 0.7104848 0.3990748 0.2802669
#>          F3         F4       F5        F6        F7        F9        F10
#> 1 0.3814154 0.04832029 0.381568 0.1507689 0.3692636 0.6485493 0.06645404
#> 2 0.3814154 0.04832029 0.381568 0.1507689 0.3692636 0.6485493 0.06645404
#> 3 0.3814154 0.04832029 0.381568 0.5995262 0.3692636 0.3319902 0.06645404
#> 4 0.3814154 0.04832029 0.381568 0.1507689 0.3692636 0.6485493 0.06645404
#>         approach
#> 1    abs epsilon
#> 2       wtd goal
#> 3 Chebyshev goal
#> 4      ref point
# }
```
