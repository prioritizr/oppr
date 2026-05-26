# Add manual targets

Add targets to a project prioritization problem by manually specifying
detailed information for each target threshold. Although this function
is useful because it can be used to customize all aspects of a target,
it requires considerable more information that other functions for
adding targets (e.g.,
[`add_absolute_targets()`](https://prioritizr.github.io/oppr/reference/add_absolute_targets.md)
and
[`add_relative_targets()`](https://prioritizr.github.io/oppr/reference/add_relative_targets.md)).

## Usage

``` r
add_manual_targets(x, targets)

# S4 method for class 'ProjectProblem,data.frame'
add_manual_targets(x, targets)

# S4 method for class 'ProjectProblem,tbl_df'
add_manual_targets(x, targets)
```

## Arguments

- x:

  \[problem() object.

- targets:

  `data.frame` or
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object. See the Details section for more information.

## Value

A [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
object with the targets added to it.

## Details

Targets are used to specify a threshold minimum desirable expected
outcome for each feature. These should ideally be set according to
stakeholder requirements and expert knowledge. Please note that
attempting to solve problems with objectives that require targets
without specifying targets will throw an error.

The argument to `targets` should contain the following columns:

- `"feature"`:

  `character` values with names of features in `x`.

- `"type"`:

  `character` values describing the type of target. Acceptable values
  include `"absolute"` and `"relative"`.

- `"sense"`:

  `character` values indicating the constraint sense for the target. The
  only acceptable value currently supported is: `">="`. This field
  (column) is optional and if it is missing then target senses will
  default to `">="` values.

- `"target"`:

  `numeric` target threshold.

## See also

See [targets](https://prioritizr.github.io/oppr/reference/targets.md)
for an overview for functions for adding targets.

Other targets:
[`add_absolute_targets()`](https://prioritizr.github.io/oppr/reference/add_absolute_targets.md),
[`add_relative_targets()`](https://prioritizr.github.io/oppr/reference/add_relative_targets.md)

## Examples

``` r
# load data
data(sim_projects, sim_features, sim_actions)

# create data frame with targets
targets <- data.frame(
  feature = sim_features$name,
  type = "absolute",
  target = 0.1
)

# print targets
print(targets)
#>   feature     type target
#> 1      F1 absolute    0.1
#> 2      F2 absolute    0.1
#> 3      F3 absolute    0.1
#> 4      F4 absolute    0.1
#> 5      F5 absolute    0.1

# build problem with minimum set objective and targets that require each
# feature to have a 30% chance of persisting into the future
p <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_min_set_objective() %>%
  add_manual_targets(targets) %>%
  add_binary_decisions()

# print problem
print(p)
#> Project Prioritization Problem
#> actions:         F1_action, F2_action, F3_action, ... (6 actions)
#> projects:        F1_project, F2_project, F3_project, ... (6 projects)
#> features:        F1, F2, F3, ... (5 features)
#> action costs:    continuous values (between 0 and 103.226)
#> project success: proportion values (between 0.814 and 1)
#> objective:       minimum set objective
#> targets:         absolute targets
#> weights:         none specified
#> constraints:     none specified
#> decisions:       binary decision
#> solver:          none specified

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
#> Optimize a model with 26 rows, 22 columns and 52 nonzeros (Min)
#> Model fingerprint: 0xab43a3a6
#> Model has 5 linear objective coefficients
#> Variable types: 0 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+00]
#>   Objective range  [9e+01, 1e+02]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [1e-01, 1e+00]
#> 
#> Found heuristic solution: objective 304.1251127
#> Presolve removed 13 rows and 8 columns
#> Presolve time: 0.00s
#> Presolved: 13 rows, 14 columns, 26 nonzeros
#> Variable types: 0 continuous, 14 integer (14 binary)
#> Root relaxation presolved: 13 rows, 14 columns, 26 nonzeros
#> 
#> 
#> Root relaxation: objective 1.032258e+02, 10 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0     103.2258290  103.22583  0.00%     -    0s
#> 
#> Explored 1 nodes (10 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 103.226 
#> No other solutions better than 103.226
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 1.032258290263e+02, best bound 1.032258290263e+02, gap 0.0000%

# print solution
print(s)
#> # A tibble: 1 × 21
#>   solution status   cost   obj F1_action F2_action F3_action F4_action F5_action
#>      <int> <chr>   <dbl> <dbl> <lgl>     <lgl>     <lgl>     <lgl>     <lgl>    
#> 1        1 OPTIMAL  103.  103. FALSE     FALSE     TRUE      FALSE     FALSE    
#> # ℹ 12 more variables: baseline_action <lgl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>
```
