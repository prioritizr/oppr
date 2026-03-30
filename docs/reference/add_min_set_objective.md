# Add minimum set objective

Add an objective to a project prioritization problem based on minimizing
the cost of the solution, whilst ensuring that the target thresholds for
each feature is met (Chadés *et al.* 2015). In some project
prioritization exercises, decision makers may have a target threshold
level of expected outcome for each feature (e.g., a 90% chance of
persistence for each feature). This objective is especially useful for
identifying solutions that ensure that each feature achieves, at least,
some minimum level of expected outcome based on the target thresholds.

## Usage

``` r
add_min_set_objective(x)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  object.

## Value

A [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
with the objective added to it.

## Details

A problem objective is used to specify the overall goal of the project
prioritization problem. Here, the minimum set objective seeks to find
the set of actions that minimizes the overall cost of the
prioritization, while ensuring that the funded projects meet a set of
targets for the expected outcomes (e.g., probabilities of persistence,
population sizes, amount of habitat) associated with conservation
features (e.g., populations, species, ecosystems). Let \\I\\ represent
the set of conservation actions (indexed by \\i\\). Let \\C_i\\ denote
the cost for funding action \\i\\. Also, let \\F\\ represent each
feature (indexed by \\f\\), \\T_f\\ represent the target for feature
\\f\\, and \\E_f\\ denote the expected outcome for each feature given
the funded conservation projects.

To guide the prioritization, the conservation actions are organized into
conservation projects. Let \\J\\ denote the set of conservation projects
(indexed by \\j\\), and let \\A\_{ij}\\ denote which actions \\i \in I\\
comprise each conservation project \\j \in J\\ using zeros and ones.
Next, let \\P_j\\ represent the probability of project \\j\\ being
successful if it is funded. Also, let \\B\_{fj}\\ denote the outcome for
each feature \\f \in F\\ associated with the project \\j \in J\\
assuming that all of the actions comprising project \\j\\ are funded and
that project is allocated to feature \\f\\. For convenience, let
\\Q\_{fj}\\ denote the expected outcome for each \\f \in F\\ associated
with the project \\j \in J\\ if the project is funded. If the argument
to `adjust_for_baseline` in the `problem` function was set to `TRUE`,
and this is the default behavior, then \\Q\_{fj} = (P\_{j} \times
B\_{fj}) + \bigg(\big(1 - (P\_{j} B\_{fj})\big) \times (P\_{n} \times
B\_{fn})\bigg)\\, where `n` corresponds to the baseline "do nothing"
project. This means that the expected outcome for a feature given that a
project is funded and allocated to the feature depends on (i) the
probability of the project succeeding, (ii) the outcome for the feature
if the project succeeds, and (iii) the outcome for the feature if the
project fails (per the baseline project). Otherwise, if the argument is
set to `FALSE`, then \\Q\_{fj} = P\_{j} \times B\_{fj}\\.

The binary control variables \\X_i\\ in this problem indicate whether
each project \\i \in I\\ is funded or not. The decision variables in
this problem are the \\Y\_{j}\\, \\Z\_{fj}\\, and \\E_f\\ variables.
Specifically, the binary \\Y\_{j}\\ variables indicate if project \\j\\
is funded or not based on which actions are funded; the binary
\\Z\_{fj}\\ variables indicate if project \\j\\ is used to manage
feature \\f\\ or not; and the continuous \\E_f\\ variables denote the
expected outcome for feature \\f\\.

Now that we have defined all the data and variables, we can formulate
the problem. For convenience, let the symbol used to denote each set
also represent its cardinality (e.g., if there are ten features, let
\\F\\ represent the set of ten features and also the number ten).

\$\$ \mathrm{Minimize} \space \sum\_{i = 0}^{I} C_i X_i \space
\mathrm{(eqn \space 1a)} \\ \mathrm{Subject \space to} \space \\ E_f
\geq T_f \space \forall f \in F \space \mathrm{(eqn \space 1b)} \\ E_f =
\sum\_{j = 0}^{J} Z\_{fj} Q\_{fj} \space \forall \space f \in F \space
\mathrm{(eqn \space 1c)} \\ Z\_{fj} \leq Y\_{j} \space \forall \space j
\in J \space \mathrm{(eqn \space 1d)} \\ \sum\_{j = 0}^{J} Z\_{fj}
\times \mathrm{ceil}(Q\_{fj}) = 1 \space \forall \space f \in F \space
\mathrm{(eqn \space 1e)} \\ A\_{ij} Y\_{j} \leq X\_{i} \space \forall
\space i \in I, j \in J \space \mathrm{(eqn \space 1f)} \\ E\_{f} \geq 0
\space \forall \space f \in F \space \mathrm{(eqn \space 1g)} \\ X\_{i},
Y\_{j}, Z\_{fj} \in \\0, 1\\ \space \forall \space i \in I, j \in J, f
\in F \space \mathrm{(eqn \space 1h)} \$\$

The objective (eqn 1a) is to minimize the cost of the funded actions.
Constraints (eqn 1b) ensure that the targets are met. Constraints (eqn
1c) calculate the expected outcome for each feature according to their
allocated project. Constraints (eqn 1d) ensure that feature can only be
allocated to projects that have all of their actions funded. Constraints
(eqn 1e) state that each feature can only be allocated to a single
project. Constraints (eqn 1f) ensure that a project cannot be funded
unless all of its actions are funded. Constraints (eqns 1g) ensure that
the expected outcome variables (\\E_f\\) are greater than zero.
Constraints (eqns 1h) ensure that the action funding (\\X_i\\), project
funding (\\Y_j\\), and project allocation (\\Z\_{fj}\\) variables are
binary.

## References

Chadés I, Nicol S, van Leeuwen S, Walters B, Firn J, Reeson A, Martin TG
& Carwardine J (2015) Benefits of integrating complementarity into
priority threat management. Conservation Biology **29**: 525–536.

## See also

See [targets](https://prioritizr.github.io/oppr/reference/targets.md)
for an overview of functions for adding targets.

Other objectives:
[`add_max_phylo_div_objective()`](https://prioritizr.github.io/oppr/reference/add_max_phylo_div_objective.md),
[`add_max_richness_objective()`](https://prioritizr.github.io/oppr/reference/add_max_richness_objective.md),
[`add_max_targets_met_objective()`](https://prioritizr.github.io/oppr/reference/add_max_targets_met_objective.md),
[`add_max_wtd_sum_objective()`](https://prioritizr.github.io/oppr/reference/add_max_wtd_sum_objective.md)

## Examples

``` r
# \dontrun{
# load the ggplot2 R package to customize plot
library(ggplot2)

# load data
data(sim_projects, sim_features, sim_actions)

# build problem with minimum set objective and targets that require each
# feature to have a 30% chance of persisting into the future
p <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_min_set_objective() %>%
  add_absolute_targets(0.3) %>%
  add_binary_decisions()

# solve problem
s <- solve(p)
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
#> Optimize a model with 26 rows, 22 columns and 52 nonzeros (Min)
#> Model fingerprint: 0x21fb06c6
#> Model has 5 linear objective coefficients
#> Variable types: 0 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+00]
#>   Objective range  [9e+01, 1e+02]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [3e-01, 1e+00]
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

# print solution
print(s)
#> # A tibble: 1 × 21
#>   solution status   cost   obj F1_action F2_action F3_action F4_action F5_action
#>      <int> <chr>   <dbl> <dbl> <lgl>     <lgl>     <lgl>     <lgl>     <lgl>    
#> 1        1 OPTIMAL  498.  498. TRUE      TRUE      TRUE      TRUE      TRUE     
#> # ℹ 12 more variables: baseline_action <lgl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>

# plot solution, and add a dashed line to indicate the feature targets
plot(p, s) + geom_hline(yintercept = 0.3, linetype = "dashed")

# }
```
