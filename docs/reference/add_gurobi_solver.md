# Add a *Gurobi* solver

Add a solver to generate solutions to a project prioritization problem
with the [*Gurobi*](https://www.gurobi.com) software. This function can
also be used to customize the behavior of the solver. See below for
details on installation requirements.

## Usage

``` r
add_gurobi_solver(
  x,
  gap = 0,
  number_solutions = 1,
  solution_pool_method = 2,
  time_limit = .Machine$integer.max,
  presolve = 2,
  threads = 1,
  first_feasible = FALSE,
  numeric_focus = 0,
  verbose = TRUE
)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  or
  [`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
  object.

- gap:

  `numeric` gap to optimality. This gap is relative and expresses the
  acceptable deviance from the optimal objective. For example, a value
  of 0.01 will result in the solver stopping when it has found a
  solution within 1% of optimality. Additionally, a value of 0 will
  result in the solver stopping when it has found an optimal solution.
  The default value is 0 (i.e., 0% from optimality).

- number_solutions:

  `integer` number of solutions desired. Defaults to 1. Note that the
  number of returned solutions can sometimes be less than the argument
  to `number_solutions` depending on the argument to
  `solution_pool_method`, for example if 100 solutions are requested but
  only 10 unique solutions exist, then only 10 solutions will be
  returned.

- solution_pool_method:

  `numeric` search method identifier that determines how multiple
  solutions should be generated. Available search modes for generating a
  portfolio of solutions include: `0` recording all solutions identified
  whilst trying to find a solution that is within the specified
  optimality gap, `1` finding one solution within the optimality gap and
  a number of additional solutions that are of any level of quality
  (such that the total number of solutions is equal to
  `number_solutions`), and `2` finding a specified number of solutions
  that are nearest to optimality. For more information, see the *Gurobi*
  manual (i.e.,
  <https://docs.gurobi.com/projects/optimizer/en/current/reference/parameters.html#poolsearchmode>).
  Defaults to 2.

- time_limit:

  `numeric` time limit in seconds to run the optimizer. The solver will
  return the current best solution when this time limit is exceeded.

- presolve:

  `integer` number indicating how intensively the solver should try to
  simplify the problem before solving it. The default value of 2
  indicates to that the solver should be very aggressive in trying to
  simplify the problem.

- threads:

  `integer` number of threads to use for the optimization algorithm. The
  default value of 1 will result in only one thread being used.

- first_feasible:

  `logical` should the first feasible solution be be returned? If
  `first_feasible` is set to `TRUE`, the solver will return the first
  solution it encounters that meets all the constraints, regardless of
  solution quality. Note that the first feasible solution is not an
  arbitrary solution, rather it is derived from the relaxed solution,
  and is therefore often reasonably close to optimality. Defaults to
  `FALSE`.

- numeric_focus:

  `integer` value denoting how much extra attention be paid to verifying
  the accuracy of numerical calculations? Acceptable values include 0,
  1, 2, or 3. This may be useful when dealing with problems that may
  suffer from numerical instability issues. Beware that setting greater
  values will likely increase run time. Defaults to 0.

- verbose:

  `logical` should information be printed during optimization? Defaults
  to `TRUE`.

## Value

A [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
object with the solver added to it.

## Details

[*Gurobi*](https://www.gurobi.com) is a state-of-the-art commercial
optimization software with an R package interface. It is by far the
fastest of the solvers supported by this package, however, it is also
the only solver that is not freely available. That said, licenses are
available to academics at no cost. The gurobi package is distributed
with the *Gurobi* software suite. This solver uses the gurobi package to
solve problems.

To install the gurobi package, the [Gurobi](https://www.gurobi.com)
optimization suite will first need to be installed (see
<https://support.gurobi.com/hc/en-us/articles/4534161999889-How-do-I-install-Gurobi-Optimizer>
for instructions). Although [Gurobi](https://www.gurobi.com) is a
commercial software, academics can obtain a [special license for no
cost](https://www.gurobi.com/downloads/end-user-license-agreement-academic/).
After installing the [Gurobi](https://www.gurobi.com) optimization
suite, the gurobi package can then be installed (see
<https://support.gurobi.com/hc/en-us/articles/14462206790033-How-do-I-install-Gurobi-for-R>
for instructions).

## See also

See [solvers](https://prioritizr.github.io/oppr/reference/solvers.md)
for an overview of functions for adding solvers.

Other solvers:
[`add_default_solver()`](https://prioritizr.github.io/oppr/reference/add_default_solver.md),
[`add_heuristic_solver()`](https://prioritizr.github.io/oppr/reference/add_heuristic_solver.md),
[`add_highs_solver()`](https://prioritizr.github.io/oppr/reference/add_highs_solver.md),
[`add_lpsolveapi_solver()`](https://prioritizr.github.io/oppr/reference/add_lpsolveapi_solver.md),
[`add_lpsymphony_solver()`](https://prioritizr.github.io/oppr/reference/add_lpsymphony_solver.md),
[`add_random_solver()`](https://prioritizr.github.io/oppr/reference/add_random_solver.md),
[`add_rsymphony_solver()`](https://prioritizr.github.io/oppr/reference/add_rsymphony_solver.md)

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

# build another problem, and specify the Gurobi solver
p2 <- p1 %>% add_gurobi_solver()

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
#> constraints:     none specified
#> decisions:       binary decision
#> solver:          gurobi solver

# solve problem
s2 <- solve(p2)
#> Set parameter Username
#> Set parameter LicenseID to value 2774703
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0
#> Set parameter ScaleFlag to value 2
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

# print solution
print(s2)
#> # A tibble: 1 × 21
#>   solution status   cost   obj F1_action F2_action F3_action F4_action F5_action
#>      <int> <chr>   <dbl> <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#> 1        1 OPTIMAL  195.  2.19         1         1         0         0         0
#> # ℹ 12 more variables: baseline_action <dbl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>

# plot solution
plot(p2, s2)


# build another problem and obtain multiple solutions
# note that this problem doesn't have 100 unique solutions so
# the solver won't return 100 solutions
p3 <- p1 %>% add_gurobi_solver(number_solutions = 100)

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
#> constraints:     none specified
#> decisions:       binary decision
#> solver:          gurobi solver

# solve problem
s3 <- solve(p3)
#> Set parameter Username
#> Set parameter LicenseID to value 2774703
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0
#> Set parameter ScaleFlag to value 2
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter PoolSolutions to value 100
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
#> Presolve  2
#> Threads  1
#> PoolSolutions  100
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
#> Found heuristic solution: objective 1.9149015
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
#> Optimal solution found at node 0 - now completing solution pool...
#> 
#>     Nodes    |    Current Node    |      Pool Obj. Bounds     |     Work
#>              |                    |   Worst                   |
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0               -    2.19038      -     -    0s
#>      0     0          -    0               -    2.19038      -     -    0s
#>      0     0          -    0               -    2.19038      -     -    0s
#>      0     2          -    0               -    2.19038      -     -    0s
#> 
#> Explored 121 nodes (82 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 61: 2.19038 2.01465 1.98593 ... 1.06521
#> No other solutions better than 1.06521
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.190380737245e+00, best bound 2.190380737245e+00, gap 0.0000%

# print solutions
print(s3)
#> # A tibble: 11 × 21
#>    solution status  cost   obj F1_action F2_action F3_action F4_action F5_action
#>       <int> <chr>  <dbl> <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1        1 OPTIM… 195.   2.19         1         1         0         0         0
#>  2        2 SUBOP… 194.   2.01         1         0         0         1         0
#>  3        3 SUBOP… 194.   1.99         1         0         0         0         1
#>  4        4 SUBOP… 198.   1.96         1         0         1         0         0
#>  5        5 SUBOP… 199.   1.91         0         0         0         1         1
#>  6        6 SUBOP… 101.   1.68         0         1         0         0         0
#>  7        7 SUBOP…  94.4  1.58         1         0         0         0         0
#>  8        8 SUBOP…  99.2  1.50         0         0         0         1         0
#>  9        9 SUBOP…  99.9  1.48         0         0         0         0         1
#> 10       10 SUBOP… 103.   1.45         0         0         1         0         0
#> 11       11 SUBOP…   0    1.07         0         0         0         0         0
#> # ℹ 12 more variables: baseline_action <dbl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>
# }
```
