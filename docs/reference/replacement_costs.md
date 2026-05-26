# Replacement cost

Calculate the replacement cost for priority actions in a project
prioritization
[`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
(Moilanen *et al.* 2009). Actions associated with larger replacement
cost values are more irreplaceable, and may need to be implemented
sooner than actions with lower replacement cost values.

## Usage

``` r
replacement_costs(x, solution, n = 1)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  or
  [`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
  object.

- solution:

  [`base::data.frame()`](https://rdrr.io/r/base/data.frame.html) or
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing the solutions. Here, rows correspond to different solutions
  and columns correspond to different actions. Each column in the
  argument to `solution` should be named according to a different action
  in `x`. Cell values indicate if an action is funded in a given
  solution or not, and should be either zero or one. Arguments to
  `solution` can contain additional columns, though they will be
  ignored.

- n:

  `integer` solution number to calculate replacement cost values. Since
  each row in the argument to `solutions` corresponds to a different
  solution, this argument should correspond to a row in the argument to
  `solutions`. Defaults to 1.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
table containing the following columns.

- `"action"`:

  `character` name of each action.

- `"cost"`:

  `numeric` cost of each solution when each action is locked out.

- `"obj"`:

  `numeric` objective value of each solution when each action is locked
  out. This is calculated using the objective function defined for the
  argument to `x`.

- `"rep_cost"`:

  `numeric` replacement cost for each action. Greater values indicate
  greater irreplaceability. Missing (`NA`) values are assigned to
  actions which are not selected for funding in the specified solution,
  infinite (`Inf`) values are assigned to to actions which are required
  to meet feasibility constraints, and negative values mean that
  superior solutions than the specified solution exist.

## Details

Replacement cost values are calculated for each priority action
specified in the solution. Missing (`NA`) values are assigned to actions
which are not selected for funding in the specified solution. For a
given action, its replacement cost is calculated by (i) calculating the
objective value for the optimal solution to the argument to `x`, (ii)
calculating the objective value for the optimal solution to the argument
to `x` with the given action locked out, (iii) calculating the
difference between the two objective values, (iv) the problem has an
objective which aims to minimize the objective value (only
[`add_min_set_objective()`](https://prioritizr.github.io/oppr/reference/add_min_set_objective.md),
then the resulting value is multiplied by minus one so that larger
values always indicate actions with greater irreplaceability. Please
note this function can take a long time to complete for large problems
since it involves re-solving the problem for every action selected for
funding.

## References

Moilanen A, Arponen A, Stokland JN & Cabeza M (2009) Assessing
replacement cost of conservation areas: how does habitat loss influence
priorities? *Biological Conservation*, **142**, 575–585.

## See also

Other functions for evaluating solutions:
[`project_cost_effectiveness()`](https://prioritizr.github.io/oppr/reference/project_cost_effectiveness.md),
[`rank_importance()`](https://prioritizr.github.io/oppr/reference/rank_importance.md),
[`solution_statistics()`](https://prioritizr.github.io/oppr/reference/solution_statistics.md)

## Examples

``` r
# load data
data(sim_projects, sim_features, sim_actions)

# build problem with maximum weighted sum objective and $400 budget
p <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_max_wtd_sum_objective(budget = 400) %>%
  add_feature_weights("weight") %>%
  add_binary_decisions()

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
#> Optimize a model with 27 rows, 27 columns and 62 nonzeros (Max)
#> Model fingerprint: 0x85f2486a
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [2e-01, 2e+00]
#>   Bounds range     [5e-01, 1e+00]
#>   RHS range        [1e+00, 4e+02]
#> 
#> Found heuristic solution: objective 0.6654645
#> Presolve removed 16 rows and 12 columns
#> Presolve time: 0.00s
#> Presolved: 11 rows, 15 columns, 24 nonzeros
#> Variable types: 0 continuous, 15 integer (15 binary)
#> Root relaxation presolved: 11 rows, 15 columns, 24 nonzeros
#> 
#> 
#> Root relaxation: objective 1.749045e+00, 11 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       1.7490448    1.74904  0.00%     -    0s
#> 
#> Explored 1 nodes (11 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 1.74904 
#> No other solutions better than 1.74904
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 1.749044775334e+00, best bound 1.749044775334e+00, gap 0.0000%

# print solution
print(s)
#> # A tibble: 1 × 21
#>   solution status   cost   obj F1_action F2_action F3_action F4_action F5_action
#>      <int> <chr>   <dbl> <dbl> <lgl>     <lgl>     <lgl>     <lgl>     <lgl>    
#> 1        1 OPTIMAL  395.  1.75 TRUE      TRUE      FALSE     TRUE      TRUE     
#> # ℹ 12 more variables: baseline_action <lgl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>

# calculate replacement cost values
r <- replacement_costs(p, s)

# print output
print(r)
#> # A tibble: 6 × 4
#>   name             cost    obj  rep_cost
#>   <chr>           <dbl>  <dbl>     <dbl>
#> 1 F1_action        300.   1.64    0.108 
#> 2 F2_action        397.   1.70    0.0458
#> 3 F3_action         NA   NA      NA     
#> 4 F4_action        399.   1.56    0.192 
#> 5 F5_action        398.   1.18    0.569 
#> 6 baseline_action  Inf  Inf    -Inf     

# plot histogram of replacement costs,
# with this objective, greater values indicate greater irreplaceability
hist(r$rep_cost, xlab = "Replacement cost", main = "")
```
