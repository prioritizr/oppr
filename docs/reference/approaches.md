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
#> 
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
#>      0     0    0.72112    0    3    0.68124    0.72112  5.85%     -    0s
#>      0     0    0.72112    0    3    0.68124    0.72112  5.85%     -    0s
#>      0     0    0.72047    0    6    0.68124    0.72047  5.76%     -    0s
#>      0     0    0.71955    0    9    0.68124    0.71955  5.62%     -    0s
#> H    0     0                       0.7103971    0.71946  1.28%     -    0s
#>      0     0    0.71946    0    9    0.71040    0.71946  1.28%     -    0s
#>      0     0    0.71946    0    8    0.71040    0.71946  1.28%     -    0s
#> H    0     0                       0.7120817    0.71937  1.02%     -    0s
#>      0     0    0.71937    0   11    0.71208    0.71937  1.02%     -    0s
#>      0     0    0.71937    0    9    0.71208    0.71937  1.02%     -    0s
#>      0     0    0.71928    0    9    0.71208    0.71928  1.01%     -    0s
#>      0     0    0.71928    0   11    0.71208    0.71928  1.01%     -    0s
#>      0     0    0.71928    0   13    0.71208    0.71928  1.01%     -    0s
#>      0     0    0.71928    0   15    0.71208    0.71928  1.01%     -    0s
#>      0     0    0.71915    0    9    0.71208    0.71915  0.99%     -    0s
#>      0     0    0.71902    0    9    0.71208    0.71902  0.97%     -    0s
#>      0     0    0.71902    0   11    0.71208    0.71902  0.97%     -    0s
#>      0     0    0.71902    0   13    0.71208    0.71902  0.97%     -    0s
#>      0     0    0.71902    0    7    0.71208    0.71902  0.97%     -    0s
#>      0     0    0.71902    0    9    0.71208    0.71902  0.97%     -    0s
#>      0     0    0.71901    0   13    0.71208    0.71901  0.97%     -    0s
#>      0     0    0.71899    0    9    0.71208    0.71899  0.97%     -    0s
#>      0     0    0.71896    0   11    0.71208    0.71896  0.97%     -    0s
#>      0     0    0.71895    0   15    0.71208    0.71895  0.96%     -    0s
#>      0     0    0.71894    0   17    0.71208    0.71894  0.96%     -    0s
#>      0     0    0.71891    0   11    0.71208    0.71891  0.96%     -    0s
#>      0     0    0.71891    0   15    0.71208    0.71891  0.96%     -    0s
#>      0     0    0.71890    0   13    0.71208    0.71890  0.96%     -    0s
#>      0     0    0.71890    0   17    0.71208    0.71890  0.96%     -    0s
#>      0     0    0.71885    0   19    0.71208    0.71885  0.95%     -    0s
#>      0     0    0.71880    0   19    0.71208    0.71880  0.94%     -    0s
#>      0     0    0.71869    0   11    0.71208    0.71869  0.93%     -    0s
#>      0     0    0.71867    0   19    0.71208    0.71867  0.93%     -    0s
#>      0     0    0.71864    0   13    0.71208    0.71864  0.92%     -    0s
#>      0     0    0.71863    0   19    0.71208    0.71863  0.92%     -    0s
#>      0     0    0.71863    0   17    0.71208    0.71863  0.92%     -    0s
#>      0     0    0.71863    0   17    0.71208    0.71863  0.92%     -    0s
#>      0     0    0.71863    0   17    0.71208    0.71863  0.92%     -    0s
#>      0     2    0.71856    0   17    0.71208    0.71856  0.91%     -    0s
#> 
#> Cutting planes:
#>   Gomory: 1
#>   Cover: 14
#>   Implied bound: 2
#>   MIR: 18
#> 
#> Explored 134 nodes (782 simplex iterations) in 0.08 seconds (0.05 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.712082 
#> No other solutions better than 0.712082
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 7.120816581805e-01, best bound 7.120816581805e-01, gap 0.0000%
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
#> 
#> Presolve removed 242 rows and 79 columns
#> Presolve time: 0.00s
#> Presolved: 291 rows, 283 columns, 981 nonzeros
#> Variable types: 102 continuous, 181 integer (181 binary)
#> Found heuristic solution: objective 1.0399249
#> Found heuristic solution: objective 0.9411183
#> Root relaxation presolved: 291 rows, 283 columns, 981 nonzeros
#> 
#> 
#> Root relaxation: objective 5.569304e-01, 139 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.55693    0   16    0.94112    0.55693  40.8%     -    0s
#> H    0     0                       0.7471329    0.55693  25.5%     -    0s
#>      0     0    0.61876    0   25    0.74713    0.61876  17.2%     -    0s
#>      0     0    0.62504    0   22    0.74713    0.62504  16.3%     -    0s
#> H    0     0                       0.7424145    0.63090  15.0%     -    0s
#>      0     0    0.69778    0   21    0.74241    0.69778  6.01%     -    0s
#>      0     0    0.69778    0   13    0.74241    0.69778  6.01%     -    0s
#>      0     0    0.69778    0   19    0.74241    0.69778  6.01%     -    0s
#>      0     0    0.70734    0    4    0.74241    0.70734  4.72%     -    0s
#>      0     0    0.70742    0    7    0.74241    0.70742  4.71%     -    0s
#>      0     0    0.70759    0    5    0.74241    0.70759  4.69%     -    0s
#>      0     0    0.70768    0    7    0.74241    0.70768  4.68%     -    0s
#>      0     0    0.70772    0    5    0.74241    0.70772  4.67%     -    0s
#>      0     0    0.70780    0    7    0.74241    0.70780  4.66%     -    0s
#>      0     0    0.70780    0    7    0.74241    0.70780  4.66%     -    0s
#> H    0     0                       0.7401789    0.70780  4.37%     -    0s
#> H    0     0                       0.7359574    0.70780  3.83%     -    0s
#>      0     0    0.70784    0    3    0.73596    0.70784  3.82%     -    0s
#>      0     0    0.70784    0    1    0.73596    0.70784  3.82%     -    0s
#>      0     0    0.70784    0    5    0.73596    0.70784  3.82%     -    0s
#>      0     0    0.70822    0    1    0.73596    0.70822  3.77%     -    0s
#>      0     0    0.70829    0    5    0.73596    0.70829  3.76%     -    0s
#>      0     0    0.70838    0    7    0.73596    0.70838  3.75%     -    0s
#> H    0     0                       0.7162957    0.70841  1.10%     -    0s
#>      0     0    0.70849    0    7    0.71630    0.70849  1.09%     -    0s
#>      0     0    0.70860    0    9    0.71630    0.70860  1.07%     -    0s
#>      0     0    0.70862    0   12    0.71630    0.70862  1.07%     -    0s
#>      0     0    0.70865    0    7    0.71630    0.70865  1.07%     -    0s
#>      0     0    0.70872    0    5    0.71630    0.70872  1.06%     -    0s
#>      0     0    0.70873    0   11    0.71630    0.70873  1.06%     -    0s
#>      0     0    0.70885    0    9    0.71630    0.70885  1.04%     -    0s
#>      0     0    0.70886    0   11    0.71630    0.70886  1.04%     -    0s
#>      0     0    0.70891    0    7    0.71630    0.70891  1.03%     -    0s
#>      0     0    0.70892    0   13    0.71630    0.70892  1.03%     -    0s
#>      0     0    0.70894    0   13    0.71630    0.70894  1.03%     -    0s
#>      0     0    0.70894    0   13    0.71630    0.70894  1.03%     -    0s
#>      0     0    0.70896    0   17    0.71630    0.70896  1.02%     -    0s
#>      0     0    0.70901    0    5    0.71630    0.70901  1.02%     -    0s
#>      0     0    0.70902    0    9    0.71630    0.70902  1.02%     -    0s
#>      0     0    0.70911    0    7    0.71630    0.70911  1.00%     -    0s
#>      0     0    0.70911    0    9    0.71630    0.70911  1.00%     -    0s
#>      0     0    0.70911    0    9    0.71630    0.70911  1.00%     -    0s
#>      0     0    0.70914    0   13    0.71630    0.70914  1.00%     -    0s
#>      0     0    0.70917    0   11    0.71630    0.70917  1.00%     -    0s
#>      0     0    0.70917    0   17    0.71630    0.70917  0.99%     -    0s
#>      0     0    0.70918    0    7    0.71630    0.70918  0.99%     -    0s
#>      0     0    0.70918    0    9    0.71630    0.70918  0.99%     -    0s
#>      0     0    0.70924    0   15    0.71630    0.70924  0.99%     -    0s
#>      0     0    0.70925    0   17    0.71630    0.70925  0.98%     -    0s
#>      0     0    0.70935    0    9    0.71630    0.70935  0.97%     -    0s
#>      0     0    0.70935    0   13    0.71630    0.70935  0.97%     -    0s
#>      0     0    0.70936    0   15    0.71630    0.70936  0.97%     -    0s
#>      0     0    0.70939    0   11    0.71630    0.70939  0.96%     -    0s
#>      0     0    0.70939    0   11    0.71630    0.70939  0.96%     -    0s
#>      0     0    0.70940    0   13    0.71630    0.70940  0.96%     -    0s
#>      0     0    0.70942    0   15    0.71630    0.70942  0.96%     -    0s
#>      0     0    0.70942    0   17    0.71630    0.70942  0.96%     -    0s
#>      0     0    0.70942    0   15    0.71630    0.70942  0.96%     -    0s
#>      0     0    0.70944    0   17    0.71630    0.70944  0.96%     -    0s
#>      0     0    0.70944    0   18    0.71630    0.70944  0.96%     -    0s
#>      0     0    0.70944    0   16    0.71630    0.70944  0.96%     -    0s
#>      0     0    0.70944    0   20    0.71630    0.70944  0.96%     -    0s
#>      0     0    0.70946    0   19    0.71630    0.70946  0.95%     -    0s
#>      0     0    0.70946    0   19    0.71630    0.70946  0.95%     -    0s
#>      0     0    0.70956    0   17    0.71630    0.70956  0.94%     -    0s
#>      0     0    0.70958    0   19    0.71630    0.70958  0.94%     -    0s
#>      0     0    0.70961    0   17    0.71630    0.70961  0.93%     -    0s
#>      0     0    0.70962    0   19    0.71630    0.70962  0.93%     -    0s
#>      0     0    0.70964    0   19    0.71630    0.70964  0.93%     -    0s
#>      0     0    0.70967    0   21    0.71630    0.70967  0.93%     -    0s
#>      0     0    0.70970    0   25    0.71630    0.70970  0.92%     -    0s
#>      0     0    0.70976    0   21    0.71630    0.70976  0.91%     -    0s
#>      0     0    0.70977    0   21    0.71630    0.70977  0.91%     -    0s
#>      0     0    0.70979    0   21    0.71630    0.70979  0.91%     -    0s
#>      0     2    0.70982    0   21    0.71630    0.70982  0.90%     -    0s
#> 
#> Cutting planes:
#>   Cover: 9
#>   MIR: 21
#>   Inf proof: 1
#> 
#> Explored 82 nodes (1231 simplex iterations) in 0.13 seconds (0.09 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.716296 
#> No other solutions better than 0.716296
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 7.162957319151e-01, best bound 7.162957319151e-01, gap 0.0000%
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
#> Model fingerprint: 0x0d6d14cc
#> Model has 1 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [2e-01, 1e+03]
#> 
#> Presolve removed 243 rows and 79 columns
#> Presolve time: 0.01s
#> Presolved: 293 rows, 284 columns, 985 nonzeros
#> Variable types: 103 continuous, 181 integer (181 binary)
#> Found heuristic solution: objective 0.6115475
#> Root relaxation presolved: 293 rows, 284 columns, 985 nonzeros
#> 
#> 
#> Root relaxation: objective 3.403610e-01, 129 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.34036    0   24    0.61155    0.34036  44.3%     -    0s
#>      0     0    0.34775    0   26    0.61155    0.34775  43.1%     -    0s
#> H    0     0                       0.4283774    0.41249  3.71%     -    0s
#>      0     0    0.42646    0   14    0.42838    0.42646  0.45%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42673    0   23    0.42838    0.42673  0.39%     -    0s
#>      0     0    0.42673    0   23    0.42838    0.42673  0.38%     -    0s
#>      0     0    0.42673    0   23    0.42838    0.42673  0.38%     -    0s
#>      0     0    0.42673    0   23    0.42838    0.42673  0.38%     -    0s
#>      0     0    0.42674    0   23    0.42838    0.42674  0.38%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42687    0   24    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   20    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#> 
#> Cutting planes:
#>   Gomory: 1
#>   Cover: 1
#>   RLT: 1
#> 
#> Explored 1 nodes (675 simplex iterations) in 0.04 seconds (0.04 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.428377 
#> No other solutions better than 0.428377
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 4.283773900957e-01, best bound 4.283773900957e-01, gap 0.0000%
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
#> Model fingerprint: 0x0d6d14cc
#> Model has 1 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [2e-01, 1e+03]
#> 
#> Presolve removed 243 rows and 79 columns
#> Presolve time: 0.01s
#> Presolved: 293 rows, 284 columns, 985 nonzeros
#> Variable types: 103 continuous, 181 integer (181 binary)
#> Found heuristic solution: objective 0.6115475
#> Root relaxation presolved: 293 rows, 284 columns, 985 nonzeros
#> 
#> 
#> Root relaxation: objective 3.403610e-01, 129 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.34036    0   24    0.61155    0.34036  44.3%     -    0s
#>      0     0    0.34775    0   26    0.61155    0.34775  43.1%     -    0s
#> H    0     0                       0.4283774    0.41249  3.71%     -    0s
#>      0     0    0.42646    0   14    0.42838    0.42646  0.45%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42672    0   23    0.42838    0.42672  0.39%     -    0s
#>      0     0    0.42673    0   23    0.42838    0.42673  0.39%     -    0s
#>      0     0    0.42673    0   23    0.42838    0.42673  0.38%     -    0s
#>      0     0    0.42673    0   23    0.42838    0.42673  0.38%     -    0s
#>      0     0    0.42673    0   23    0.42838    0.42673  0.38%     -    0s
#>      0     0    0.42674    0   23    0.42838    0.42674  0.38%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42681    0   22    0.42838    0.42681  0.37%     -    0s
#>      0     0    0.42687    0   24    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   20    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#>      0     0    0.42687    0   12    0.42838    0.42687  0.35%     -    0s
#> 
#> Cutting planes:
#>   Gomory: 1
#>   Cover: 1
#>   RLT: 1
#> 
#> Explored 1 nodes (675 simplex iterations) in 0.04 seconds (0.04 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.428377 
#> No other solutions better than 0.428377
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 4.283773900957e-01, best bound 4.283773900957e-01, gap 0.0000%
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
#> Model fingerprint: 0x7f7f6d7b
#> Model has 3 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e-01, 1e+00]
#>   Bounds range     [6e-01, 3e+00]
#>   RHS range        [2e-01, 1e+03]
#> 
#> User MIP start produced solution with objective 0.850986 (0.00s)
#> User MIP start produced solution with objective 0.848736 (0.00s)
#> User MIP start produced solution with objective 0.795255 (0.01s)
#> User MIP start produced solution with objective 0.795255 (0.01s)
#> Loaded user MIP start with objective 0.795255
#> 
#> Presolve removed 244 rows and 79 columns
#> Presolve time: 0.00s
#> Presolved: 293 rows, 284 columns, 985 nonzeros
#> Variable types: 103 continuous, 181 integer (181 binary)
#> Root relaxation presolved: 293 rows, 284 columns, 985 nonzeros
#> 
#> 
#> Root relaxation: objective 5.569304e-01, 123 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    0.55693    0   16    0.79525    0.55693  30.0%     -    0s
#> H    0     0                       0.7471329    0.55693  25.5%     -    0s
#>      0     0    0.61876    0   25    0.74713    0.61876  17.2%     -    0s
#>      0     0    0.62504    0   22    0.74713    0.62504  16.3%     -    0s
#> H    0     0                       0.7424145    0.63090  15.0%     -    0s
#>      0     0    0.69778    0   21    0.74241    0.69778  6.01%     -    0s
#>      0     0    0.69778    0   18    0.74241    0.69778  6.01%     -    0s
#> H    0     0                       0.7424145    0.69778  6.01%     -    0s
#>      0     0    0.70742    0    7    0.74241    0.70742  4.71%     -    0s
#>      0     0    0.70759    0    5    0.74241    0.70759  4.69%     -    0s
#>      0     0    0.70768    0    7    0.74241    0.70768  4.68%     -    0s
#>      0     0    0.70779    0    5    0.74241    0.70779  4.66%     -    0s
#>      0     0    0.70780    0    7    0.74241    0.70780  4.66%     -    0s
#>      0     0    0.70780    0    7    0.74241    0.70780  4.66%     -    0s
#> H    0     0                       0.7401789    0.70780  4.37%     -    0s
#> H    0     0                       0.7359574    0.70780  3.83%     -    0s
#>      0     0    0.70793    0    3    0.73596    0.70793  3.81%     -    0s
#>      0     0    0.70820    0    2    0.73596    0.70820  3.77%     -    0s
#> H    0     0                       0.7162957    0.70820  1.13%     -    0s
#>      0     0    0.71537    0    2    0.71630    0.71537  0.13%     -    0s
#>      0     0    0.71593    0    6    0.71630    0.71593  0.05%     -    0s
#> 
#> Explored 1 nodes (512 simplex iterations) in 0.06 seconds (0.05 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.716296 
#> No other solutions better than 0.716296
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 7.162957317435e-01, best bound 7.162957317435e-01, gap 0.0000%
s$approach <- c("abs epsilon", "wtd goal", "Chebyshev goal", "ref point")

# print solutions
print(as.data.frame(s))
#>   solution  status     cost      obj1      obj2     obj3 A1_action A2_action
#> 1        1 OPTIMAL 983.3707 0.6701859 0.4297357 1.616604      TRUE     FALSE
#> 2        1 OPTIMAL 884.8571 0.6701859 0.4297357 1.616604      TRUE     FALSE
#> 3        1 OPTIMAL 969.5956 0.5989621 0.4297357 1.748802      TRUE      TRUE
#> 4        1 OPTIMAL 884.8571 0.6701859 0.4297357 1.616604      TRUE     FALSE
#>   A3_action A4_action A5_action A6_action A7_action A8_action A9_action
#> 1      TRUE     FALSE      TRUE      TRUE      TRUE     FALSE     FALSE
#> 2      TRUE     FALSE      TRUE      TRUE      TRUE     FALSE     FALSE
#> 3      TRUE      TRUE      TRUE     FALSE     FALSE      TRUE     FALSE
#> 4      TRUE     FALSE      TRUE      TRUE      TRUE     FALSE     FALSE
#>   A10_action A11_action A12_action A13_action A14_action A15_action B1_action
#> 1       TRUE       TRUE       TRUE      FALSE       TRUE       TRUE      TRUE
#> 2       TRUE       TRUE       TRUE      FALSE      FALSE       TRUE      TRUE
#> 3       TRUE       TRUE      FALSE       TRUE       TRUE      FALSE      TRUE
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
