# Solvers

Solvers specify the software and configuration used to generate
solutions for a project prioritization problem. By default, the best
available exact algorithm solver is used.

## Details

The following solvers can be used to generate solutions for a project
prioritization problem.

- [`add_default_solver()`](https://prioritizr.github.io/oppr/reference/add_default_solver.md):

  Add the best installed solver.

- [`add_gurobi_solver()`](https://prioritizr.github.io/oppr/reference/add_gurobi_solver.md):

  Add a solver to generate solutions with the
  [*Gurobi*](https://www.gurobi.com) software.

- [`add_highs_solver()`](https://prioritizr.github.io/oppr/reference/add_highs_solver.md):

  Add a solver to generate solutions with the
  [*HiGHS*](https://highs.dev/) software via the highs package.

- [`add_rsymphony_solver()`](https://prioritizr.github.io/oppr/reference/add_rsymphony_solver.md):

  Add a solver to generate solutions with the
  [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) software via the
  Rsymphony package.

- [`add_lpsymphony_solver()`](https://prioritizr.github.io/oppr/reference/add_lpsymphony_solver.md):

  Add a solver to generate solutions with the
  [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) software via the
  lpsymphony package.

- [`add_lpsolveapi_solver()`](https://prioritizr.github.io/oppr/reference/add_lpsolveapi_solver.md):

  Add a solver to generate solutions with the
  [*lp_solve*](https://lpsolve.sourceforge.net/5.5/) software via the
  lpSolveAPI package.

- [`add_heuristic_solver()`](https://prioritizr.github.io/oppr/reference/add_heuristic_solver.md):

  Add a solver to generate solutions using a backwards heuristic
  algorithm.

- [`add_random_solver()`](https://prioritizr.github.io/oppr/reference/add_random_solver.md):

  Add a solver to generate solutions by randomly selecting actions for
  funding.

## See also

Other overviews:
[`approaches`](https://prioritizr.github.io/oppr/reference/approaches.md),
[`constraints`](https://prioritizr.github.io/oppr/reference/constraints.md),
[`objectives`](https://prioritizr.github.io/oppr/reference/objectives.md),
[`targets`](https://prioritizr.github.io/oppr/reference/targets.md),
[`weights()`](https://prioritizr.github.io/oppr/reference/weights.md)

## Examples

``` r
# \dontrun{
# load data
data(sim_projects, sim_features, sim_actions)

# build problem
p1 <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_max_wtd_sum_objective(budget = 200) %>%
  add_binary_decisions()

# build another problem, with the default solver
p2 <- p1 %>% add_default_solver()

# build another problem, with the gurobi solver
p3 <- p1 %>% add_gurobi_solver()

# build another problem, with the highs solver
p4 <- p1 %>% add_highs_solver()

# build another problem, with the Rsymphony solver
p5 <- p1 %>% add_rsymphony_solver()

# build another problem, with the lpsymphony solver
p6 <- p1 %>% add_lpsymphony_solver()

# build another problem, with the lpSolveAPI solver
p7 <- p1 %>% add_lpsolveapi_solver()

# build another problem, with the heuristic solver
p8 <- p1 %>% add_heuristic_solver()

# build another problem, with the random solver
p9 <- p1 %>% add_random_solver()

# generate solutions using each of the solvers
s <- rbind(
  solve(p2), solve(p3), solve(p4), solve(p5), solve(p6), solve(p7),
  solve(p8), solve(p9)
)
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
#> Model fingerprint: 0x3c076626
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> Found heuristic solution: objective 1.4456093
#> Presolve removed 16 rows and 12 columns
#> Presolve time: 0.00s
#> Presolved: 11 rows, 15 columns, 25 nonzeros
#> Variable types: 0 continuous, 15 integer (15 binary)
#> Root relaxation presolved: 11 rows, 15 columns, 25 nonzeros
#> 
#> 
#> Root relaxation: objective 2.190381e+00, 11 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       2.1903807    2.19038  0.00%     -    0s
#> 
#> Explored 1 nodes (11 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 2.19038 
#> No other solutions better than 2.19038
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.190380737245e+00, best bound 2.190380737245e+00, gap 0.0000%
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
#> Model fingerprint: 0x3c076626
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#> Found heuristic solution: objective 1.4456093
#> Presolve removed 16 rows and 12 columns
#> Presolve time: 0.00s
#> Presolved: 11 rows, 15 columns, 25 nonzeros
#> Variable types: 0 continuous, 15 integer (15 binary)
#> Root relaxation presolved: 11 rows, 15 columns, 25 nonzeros
#> 
#> 
#> Root relaxation: objective 2.190381e+00, 11 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       2.1903807    2.19038  0.00%     -    0s
#> 
#> Explored 1 nodes (11 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 2.19038 
#> No other solutions better than 2.19038
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.190380737245e+00, best bound 2.190380737245e+00, gap 0.0000%
#> MIP has 27 rows; 27 cols; 62 nonzeros; 22 integer variables (22 binary)
#> 
#> Coefficient ranges:
#> 
#>   Matrix  [9e-02, 1e+02]
#> 
#>   Cost    [1e+00, 1e+00]
#> 
#>   Bound   [5e-01, 1e+00]
#> 
#>   RHS     [1e+00, 2e+02]
#> 
#> Presolving model
#> 
#> 17 rows, 10 cols, 20 nonzeros  0s
#> 
#> 6 rows, 10 cols, 15 nonzeros  0s
#> 
#> Presolve reductions: rows 6(-21); columns 10(-17); nonzeros 15(-47) 
#> 
#> 
#> Solving MIP model with:
#>    6 rows
#>    10 cols (10 binary, 0 integer, 0 implied int., 0 continuous, 0 domain fixed)
#>    15 nonzeros
#> 
#> 
#> Src: B => Branching; C => Central rounding; F => Feasibility pump; H => Heuristic;
#> 
#>      I => Shifting; J => Feasibility jump; L => Sub-MIP; P => Empty MIP; R => Randomized rounding;
#> 
#>      S => Solve LP; T => Evaluate node; U => Unbounded; X => User solution; Y => HiGHS solution;
#> 
#>      Z => ZI Round; l => Trivial lower; p => Trivial point; u => Trivial upper; z => Trivial zero
#> 
#> 
#>         Nodes      |    B&B Tree     |            Objective Bounds              |  Dynamic Constraints |       Work      
#> Src  Proc. InQueue |  Leaves   Expl. | BestBound       BestSol              Gap |   Cuts   InLp Confl. | LpIters     Time
#> 
#> 
#>  J       0       0         0   0.00%   inf             1.680145014        Large        0      0      0         0     0.0s
#> 
#>  R       0       0         0 100.00%   2.200512004     2.190380737        0.46%        0      0      0         0     0.0s
#> 
#>          1       0         1 100.00%   2.190380737     2.190380737        0.00%        0      0      0         0     0.0s
#> 
#> 
#> Solving report
#> 
#>   Status            Optimal
#>   Primal bound      2.19038073725
#>   Dual bound        2.19038073725
#>   Gap               0%
#> 
#>   P-D integral      0.000163468576509
#> 
#>   Solution status   feasible
#> 
#>                     2.19038073725 (objective)
#>                     0 (bound viol.)
#>                     0 (int. viol.)
#>                     0 (row viol.)
#> 
#>   Timing            0.01
#> 
#>   Max sub-MIP depth 0
#>   Nodes             1
#> 
#>   Repair LPs        0
#> 
#>   LP iterations     0
#> 
#> 
#> Model name:  'project prioritization problem' - run #1    
#> Objective:   Maximize(R0)
#>  
#> SUBMITTED
#> Model size:       27 constraints,      27 variables,           62 non-zeros.
#> Sets:                                   0 GUB,                  0 SOS.
#>  
#> Using DUAL simplex for phase 1 and PRIMAL simplex for phase 2.
#> The primal and dual simplex pricing strategy set to 'Devex'.
#>  
#> 
#> Relaxed solution       2.21077983146 after         27 iter is B&B base.
#>  
#> Feasible solution      1.91490153549 after         36 iter,         8 nodes (gap 9.2%)
#> Improved solution       2.0146497578 after         40 iter,        11 nodes (gap 6.1%)
#> Improved solution      2.19038073725 after         52 iter,        20 nodes (gap 0.6%)
#>  
#> Optimal solution       2.19038073725 after         52 iter,        20 nodes (gap 0.6%).
#> 
#> Excellent numeric accuracy ||*|| = 7.68052e-13
#> 
#>  MEMO: lp_solve version 5.5.2.0 for 64 bit OS, with 64 bit LPSREAL variables.
#>       In the total iteration count 52, 7 (13.5%) were bound flips.
#>       There were 11 refactorizations, 0 triggered by time and 1 by density.
#>        ... on average 4.1 major pivots per refactorization.
#>       The largest [LUSOL v2.2.1.0] fact(B) had 64 NZ entries, 1.0x largest basis.
#>       The maximum B&B level was 6, 0.1x MIP order, 4 at the optimal solution.
#>       The constraint matrix inf-norm is 103.226, with a dynamic range of 1193.9.
#>       Time to load data was 0.000 seconds, presolve used 0.000 seconds,
#>        ... 0.000 seconds in simplex solver, in total 0.000 seconds.
s$solver <- c(
  "default", "gurobi", "highs", "Rsymphony", "lpsymphony", "lpSolveAPI",
  "heuristic", "random"
)

# print solutions
print(as.data.frame(s))
#>   solution                    status     cost      obj F1_action F2_action
#> 1        1                   OPTIMAL 195.3907 2.190381         1         1
#> 2        1                   OPTIMAL 195.3907 2.190381         1         1
#> 3        1                   Optimal 195.3907 2.190381         1         1
#> 4        1 TM_OPTIMAL_SOLUTION_FOUND 195.3907 2.190381         1         1
#> 5        1 TM_OPTIMAL_SOLUTION_FOUND 195.3907 2.190381         1         1
#> 6        1    optimal solution found 195.3907 2.190381         1         1
#> 7        1                      <NA> 195.3907 2.190381         1         1
#> 8        1                      <NA> 193.6420 2.014650         1         0
#>   F3_action F4_action F5_action baseline_action F1_project F2_project
#> 1         0         0         0               1       TRUE       TRUE
#> 2         0         0         0               1       TRUE       TRUE
#> 3         0         0         0               1       TRUE       TRUE
#> 4         0         0         0               1       TRUE       TRUE
#> 5         0         0         0               1       TRUE       TRUE
#> 6         0         0         0               1       TRUE       TRUE
#> 7         0         0         0               1       TRUE       TRUE
#> 8         0         1         0               1       TRUE      FALSE
#>   F3_project F4_project F5_project baseline_project        F1        F2
#> 1      FALSE      FALSE      FALSE             TRUE 0.8080322 0.8649623
#> 2      FALSE      FALSE      FALSE             TRUE 0.8080322 0.8649623
#> 3      FALSE      FALSE      FALSE             TRUE 0.8080322 0.8649623
#> 4      FALSE      FALSE      FALSE             TRUE 0.8080322 0.8649623
#> 5      FALSE      FALSE      FALSE             TRUE 0.8080322 0.8649623
#> 6      FALSE      FALSE      FALSE             TRUE 0.8080322 0.8649623
#> 7      FALSE      FALSE      FALSE             TRUE 0.8080322 0.8649623
#> 8      FALSE       TRUE      FALSE             TRUE 0.8080322 0.2500224
#>          F3        F4        F5     solver
#> 1 0.0864612 0.2489246 0.1820005    default
#> 2 0.0864612 0.2489246 0.1820005     gurobi
#> 3 0.0864612 0.2489246 0.1820005      highs
#> 4 0.0864612 0.2489246 0.1820005  Rsymphony
#> 5 0.0864612 0.2489246 0.1820005 lpsymphony
#> 6 0.0864612 0.2489246 0.1820005 lpSolveAPI
#> 7 0.0864612 0.2489246 0.1820005  heuristic
#> 8 0.0864612 0.6881335 0.1820005     random
# }
```
