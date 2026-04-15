# Add a reference point approach

Add a reference point approach for multi-objective optimization to a
project problem (Vanderpooten 1990). Note that this function can only be
applied when all objectives should be maximized.

## Usage

``` r
add_ref_point_approach(
  x,
  weights,
  goals,
  best = NULL,
  worst = NULL,
  verbose = TRUE
)
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

- best:

  `numeric` vector containing values that denote the worst possible
  performance for each objective. If `NULL`, then these values are
  computed automatically.

- worst:

  `numeric` vector containing values that denote the worst possible
  performance for each objective. If `NULL`, then these values are
  computed automatically.

- verbose:

  `logical` should progress on generating solutions displayed? Defaults
  to `TRUE`.

## Value

A
[`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
object with the approach added to it.

## Details

The reference point approach for multi-objective optimization involves
creating a new objective that is calculated based on multiple
objectives. In particular, the new objective uses weights to specify the
relative importance of each individual objective, and goals to specify a
threshold minimum level of performance for each objective (conceptually
similar to target thresholds used in conservation planning). Given this,
the reference point approach first involves minimizing the maximum goal
shortfall (i.e., difference between goal and objective value, expressed
as a percentage), and then subsequently minimizing the weighted sum of
the goal shortfalls.

To describe this approach mathematically, we will define the following
terminology. Let \\O\\ denote the set of objectives (indexed by \\o\\).
For each objective, let \\W_o\\ denote the weight for each objective \\o
\in O\\, \\G_o\\ denote the goal for each objective \\o \in O\\, \\A_o\\
denote the best objective value for each objective, \\B_o\\ denote the
worst objective value for each objective, and \\V_o\\ denote the
objective value for a candidate solution as measured based on each
objective \\o \in O\\. After defining these terms, the approach is
formulated with the following equation.

\$\$ \mathrm{Minimize} \space \max\_{o = 0}^{O} W_o \times
\frac{1}{B_o - W_o} \times \max(G_o - V_o, 0), \\ \mathrm{Minimize}
\space \sum\_{o = 0}^{O} W_o \times \frac{1}{B_o - W_o} \times
\max(G_o - V_o, 0) \$\$

## References

Vanderpooten D (1990) *Multiobjective programming: Basic concepts and*
*approaches*. In: Stochastic Versus Fuzzy Approaches to Multiobjective
Mathematical Programming Under Uncertainty. Springer, Berlin.

## See also

See
[approaches](https://prioritizr.github.io/oppr/reference/approaches.md)
for an overview of functions for adding approaches.

Other approaches:
[`add_abs_constraint_approach()`](https://prioritizr.github.io/oppr/reference/add_abs_constraint_approach.md),
[`add_chebyshev_goal_approach()`](https://prioritizr.github.io/oppr/reference/add_chebyshev_goal_approach.md),
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
       budget = 200, tree = sim_multi_tree[[1]]
      ) %>%
      add_binary_decisions(),
   obj2 =
     problem(
       sim_multi_projects[[2]], sim_multi_actions, sim_multi_features[[2]],
       "name", "success", "name", "cost", "name",
       baseline_project_name = "baseline_project_obj2"
     ) %>%
     add_max_richness_objective(budget = 200) %>%
     add_binary_decisions(),
   obj3 =
     problem(
       sim_multi_projects[[3]], sim_multi_actions, sim_multi_features[[3]],
       "name", "success", "name", "cost", "name",
       baseline_project_name = "baseline_project_obj3"
     ) %>%
     add_max_wtd_sum_objective(budget = 200) %>%
     add_binary_decisions()
 ) %>%
 add_ref_point_approach(weights = c(10, 11, 12), goals = c(3, 4, 5)) %>%
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
#> approach:          reference point approach
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
#> Optimize a model with 338 rows, 264 columns and 1128 nonzeros (Min)
#> Model fingerprint: 0x826e74be
#> Model has 1 linear objective coefficients
#> Variable types: 15 continuous, 150 integer (150 binary)
#> Semi-Variable types: 99 continuous, 0 integer
#> Coefficient statistics:
#>   Matrix range     [2e-02, 1e+02]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [6e-01, 1e+01]
#>   RHS range        [1e+00, 2e+02]
#> 
#> Presolve removed 329 rows and 241 columns
#> Presolve time: 0.00s
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 0
#> 
#> Model is infeasible
#> Best objective -, best bound -, gap -
#> Error in solve(p): project prioritization problem is infeasible

# print solution
print(s)
#> Error: object 's' not found
# }
```
