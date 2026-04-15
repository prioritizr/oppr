# Problem objective

An objective is used to specify the overall goal of a project
prioritization problem. All project prioritization problems involve
minimizing or maximizing some kind of objective. For instance, the
decision maker may require a funding scheme that maximizes the total
number of species that are expected to persist into the future whilst
ensuring that the total cost of the funded actions does not exceed a
budget. Alternatively, the planner may require a solution that ensures
that each species meets a target level of persistence whilst minimizing
the cost of the funded actions. A project prioritization problem must
have a specified objective before it can be solved, and attempting to
solve a problem which does not have a specified objective will throw an
error.

## Details

The following objectives can be added to a conservation planning
problem.

- [`add_max_richness_objective()`](https://prioritizr.github.io/oppr/reference/add_max_richness_objective.md):

  Maximize the total number of features expected to persist into the
  future (Joseph, Maloney & Possingham 2009).

- [`add_max_phylo_div_objective()`](https://prioritizr.github.io/oppr/reference/add_max_phylo_div_objective.md):

  Maximize the phylogenetic diversity that is expected to persist into
  the future, whilst ensuring that the cost of the solution is within a
  pre-specified budget (Bennett *et al.* 2014, Faith 2008).

- [`add_max_targets_met_objective()`](https://prioritizr.github.io/oppr/reference/add_max_targets_met_objective.md):

  Maximize the total number of persistence targets met for the features,
  whilst ensuring that the cost of the solution is within a
  pre-specified budget (Chades *et al.* 2015).

- [`add_min_set_objective()`](https://prioritizr.github.io/oppr/reference/add_min_set_objective.md):

  Minimize the cost of the solution, whilst ensuring that all targets
  are met (Chadés *et al.* 2015),

- [`add_max_wtd_sum_objective()`](https://prioritizr.github.io/oppr/reference/add_max_wtd_sum_objective.md):

  Maximize the weighted sum of the expected outcomes of the features,
  whilst ensuring that the cost of the solution is within a
  pre-specified budget (Joseph, Maloney & Possingham 2009).

## References

Bennett JR, Elliott G, Mellish B, Joseph LN, Tulloch AI, Probert WJ, Di
Fonzo MMI, Monks JM, Possingham HP & Maloney R (2014) Balancing
phylogenetic diversity and species numbers in conservation
prioritization, using a case study of threatened species in New Zealand.
*Biological Conservation*, **174**: 47–54.

Chades I, Nicol S, van Leeuwen S, Walters B, Firn J, Reeson A, Martin TG
& Carwardine J (2015) Benefits of integrating complementarity into
priority threat management. *Conservation Biology*, **29**, 525–536.

Faith DP (2008) Threatened species and the potential loss of
phylogenetic diversity: conservation scenarios based on estimated
extinction probabilities and phylogenetic risk analysis. *Conservation
Biology*, **22**: 1461–1470.

Joseph LN, Maloney RF & Possingham HP (2009) Optimal allocation of
resources among threatened species: A project prioritization protocol.
*Conservation Biology*, **23**, 328–338.

## See also

Other overviews:
[`approaches`](https://prioritizr.github.io/oppr/reference/approaches.md),
[`constraints`](https://prioritizr.github.io/oppr/reference/constraints.md),
[`solvers`](https://prioritizr.github.io/oppr/reference/solvers.md),
[`targets`](https://prioritizr.github.io/oppr/reference/targets.md),
[`weights()`](https://prioritizr.github.io/oppr/reference/weights.md)

## Examples

``` r
# \dontrun{
# load data
data(sim_projects, sim_features, sim_actions, sim_tree)

# build problem
p1 <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_binary_decisions()

# build problem with maximum richness objective and $200 budget
p2 <- p1 %>% add_max_richness_objective(budget = 200)

# build problem with maximum phylogenetic diversity objective and $200 budget
p3 <- p1 %>% add_max_phylo_div_objective(budget = 200, tree = sim_tree)

# build problem with maximum targets met objective, $200 budget, and
# 40% persistence targets
p4 <-
  p1 %>%
  add_max_targets_met_objective(budget = 200) %>%
  add_absolute_targets(0.4)

# build problem with minimum set objective and 40% persistence targets
p5 <-
  p1 %>%
  add_min_set_objective() %>%
  add_absolute_targets(0.4)

# build problem with maximum weighted sum objective and $200 budget,
# note that this is identical to the maximum richness objective
# when using probability of persistence values, such as those
# present in the simulated data
p6 <- p1 %>% add_max_wtd_sum_objective(budget = 200)

# solve problems
s <- rbind(solve(p2), solve(p3), solve(p4), solve(p5), solve(p6))
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
#> Optimize a model with 27 rows, 27 columns and 62 nonzeros (Max)
#> Model fingerprint: 0x1ee444fb
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [1e+00, 1e+00]
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
#> Root relaxation: objective 2.190381e+00, 12 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       2.1903807    2.19038  0.00%     -    0s
#> 
#> Explored 1 nodes (12 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 2.19038 
#> No other solutions better than 2.19038
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.190380737245e+00, best bound 2.190380737245e+00, gap 0.0000%
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
#> Optimize a model with 30 rows, 30 columns and 83 nonzeros (Max)
#> Model fingerprint: 0xdabbf6ac
#> Model has 5 linear objective coefficients
#> Model has 3 piecewise-linear objective terms
#> Variable types: 8 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [2e-01, 2e+00]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#>   PWLObj x range   [7e-01, 5e+00]
#>   PWLObj obj range [5e-03, 1e+00]
#> 
#> Found heuristic solution: objective 1.7230322
#> Presolve removed 16 rows and 12 columns
#> Presolve time: 0.00s
#> Presolved: 17 rows, 267 columns, 289 nonzeros
#> Variable types: 252 continuous, 15 integer (15 binary)
#> Root relaxation presolved: 14 rows, 264 columns, 283 nonzeros
#> 
#> 
#> Root relaxation: objective 2.638343e+00, 22 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    2.63834    0    6    1.72303    2.63834  53.1%     -    0s
#> H    0     0                       1.9914575    2.63834  32.5%     -    0s
#> H    0     0                       2.5726522    2.63834  2.55%     -    0s
#>      0     0    2.59298    0    6    2.57265    2.59298  0.79%     -    0s
#>      0     0     cutoff    0         2.57265    2.57265  0.00%     -    0s
#> 
#> Cutting planes:
#>   MIR: 1
#> 
#> Explored 1 nodes (32 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 2.57265 
#> No other solutions better than 2.57265
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.572652172528e+00, best bound 2.572652172528e+00, gap 0.0000%
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
#> Optimize a model with 27 rows, 27 columns and 62 nonzeros (Max)
#> Model fingerprint: 0x103091f1
#> Model has 5 linear objective coefficients
#> Variable types: 0 continuous, 27 integer (27 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Found heuristic solution: objective 2.0000000
#> Presolve removed 11 rows and 7 columns
#> Presolve time: 0.00s
#> Presolved: 16 rows, 20 columns, 35 nonzeros
#> Variable types: 0 continuous, 20 integer (20 binary)
#> Root relaxation presolved: 16 rows, 20 columns, 35 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 16 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         2.00000    2.00000  0.00%     -    0s
#> 
#> Explored 1 nodes (16 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 2 
#> No other solutions better than 2
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.000000000000e+00, best bound 2.000000000000e+00, gap 0.0000%
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
#> Optimize a model with 26 rows, 22 columns and 52 nonzeros (Min)
#> Model fingerprint: 0x3a3f5e88
#> Model has 5 linear objective coefficients
#> Variable types: 0 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+00]
#>   Objective range  [9e+01, 1e+02]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [4e-01, 1e+00]
#> 
#> Found heuristic solution: objective 497.7671458
#> Presolve removed 25 rows and 20 columns
#> Presolve time: 0.00s
#> Presolved: 1 rows, 2 columns, 2 nonzeros
#> Variable types: 0 continuous, 2 integer (2 binary)
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 497.767 
#> No other solutions better than 497.767
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 4.977671458279e+02, best bound 4.977671458279e+02, gap 0.0000%
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
#> Optimize a model with 27 rows, 27 columns and 62 nonzeros (Max)
#> Model fingerprint: 0x3c076626
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
#> Root relaxation: objective 2.190381e+00, 12 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       2.1903807    2.19038  0.00%     -    0s
#> 
#> Explored 1 nodes (12 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 2.19038 
#> No other solutions better than 2.19038
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.190380737245e+00, best bound 2.190380737245e+00, gap 0.0000%

# print solutions
print(s)
#> # A tibble: 5 × 21
#>   solution status  cost    obj F1_action F2_action F3_action F4_action F5_action
#>      <int> <chr>  <dbl>  <dbl> <lgl>     <lgl>     <lgl>     <lgl>     <lgl>    
#> 1        1 OPTIM…  195.   2.19 TRUE      TRUE      FALSE     FALSE     FALSE    
#> 2        1 OPTIM…  194.   2.57 TRUE      FALSE     FALSE     FALSE     TRUE     
#> 3        1 OPTIM…  195.   2    TRUE      TRUE      FALSE     FALSE     FALSE    
#> 4        1 OPTIM…  498. 498.   TRUE      TRUE      TRUE      TRUE      TRUE     
#> 5        1 OPTIM…  195.   2.19 TRUE      TRUE      FALSE     FALSE     FALSE    
#> # ℹ 12 more variables: baseline_action <lgl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>
# }
```
