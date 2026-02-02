# Add maximum phylogenetic diversity objective

Add an objective to a project prioritization problem based on maximizing
phylogenetic diversity, whilst ensuring that the cost of the solution is
within a pre-specified budget (Bennett *et al.* 2014, Faith 2008). Note
that this objective requires that the outcome data in the
[`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
reflect probabilities of persistence.

## Usage

``` r
add_max_phylo_div_objective(x, budget, tree)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  object.

- budget:

  `numeric` value representing the maximum amount of total expenditure
  for funding actions.

- tree:

  [`ape::phylo()`](https://rdrr.io/pkg/ape/man/read.tree.html)
  phylogenetic tree describing the evolutionary relationships between
  the features. Note that `tree` must contain every feature, and only
  the features, present in `x`.

## Value

A [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
with the objective added to it.

## Details

A problem objective is used to specify the overall goal of the project
prioritization problem. Here, the maximum phylogenetic diversity
objective seeks to find the set of actions that maximizes the expected
amount of evolutionary history that is expected to persist into the
future given the evolutionary relationships between the features (e.g.,
populations, species). Let \\I\\ represent the set of conservation
actions (indexed by \\i\\). Let \\C_i\\ denote the cost for funding
action \\i\\, and let \\m\\ denote the maximum expenditure (i.e., the
budget). Also, let \\F\\ represent each feature (indexed by \\f\\),
\\W_f\\ represent the weight for each feature \\f\\ (defaults to zero
for each feature unless specified otherwise), and \\E_f\\ denote the
probability that each feature will go extinct given the funded
conservation projects.

To describe the evolutionary relationships between the features \\f \in
F\\, consider a phylogenetic tree that contains features \\f \in F\\
with branches of known lengths. This tree can be described using
mathematical notation by letting \\B\\ represent the branches (indexed
by \\b\\) with lengths \\L_b\\ and letting \\T\_{bf}\\ indicate which
features \\f \in F\\ are associated with which phylogenetic branches \\b
\in B\\ using zeros and ones. Ideally, the set of features \\F\\ would
contain all of the species in the study area – including non-threatened
species – to fully account for the benefits for funding different
actions.

To guide the prioritization, the conservation actions are organized into
conservation projects. Let \\J\\ denote the set of conservation projects
(indexed by \\j\\), and let \\A\_{ij}\\ denote which actions \\i \in I\\
comprise each conservation project \\j \in J\\ using zeros and ones.
Next, let \\P_j\\ represent the probability of project \\j\\ being
successful if it is funded. Also, let \\B\_{fj}\\ denote the enhanced
probability that each feature \\f \in F\\ associated with the project
\\j \in J\\ will persist if all of the actions that comprise project
\\j\\ are funded and that project is allocated to feature \\f\\. For
convenience, let \\Q\_{fj}\\ denote the actual probability that each \\f
\in F\\ associated with the project \\j \in J\\ is expected to persist
if the project is funded. If the argument to `adjust_for_baseline` in
the `problem` function was set to `TRUE`, and this is the default
behavior, then \\Q\_{fj} = (P\_{j} \times B\_{fj}) + \bigg(\big(1 -
(P\_{j} B\_{fj})\big) \times (P\_{n} \times B\_{fn})\bigg)\\, where `n`
corresponds to the baseline "do nothing" project. This means that the
probability of a feature persisting if a project is allocated to a
feature depends on (i) the probability of the project succeeding, (ii)
the probability of the feature persisting if the project does not fail,
and (iii) the probability of the feature persisting even if the project
fails. Otherwise, if the argument is set to `FALSE`, then \\Q\_{fj} =
P\_{j} \times B\_{fj}\\.

The binary control variables \\X_i\\ in this problem indicate whether
each project \\i \in I\\ is funded or not. The decision variables in
this problem are the \\Y\_{j}\\, \\Z\_{fj}\\, \\E_f\\, and \\R_b\\
variables. Specifically, the binary \\Y\_{j}\\ variables indicate if
project \\j\\ is funded or not based on which actions are funded; the
binary \\Z\_{fj}\\ variables indicate if project \\j\\ is used to manage
feature \\f\\ or not; the continuous \\E_f\\ variables denote the
probability that feature \\f\\ will go extinct; and the continuous
\\R_b\\ variables denote the probability that phylogenetic branch \\b\\
will remain in the future.

Now that we have defined all the data and variables, we can formulate
the problem. For convenience, let the symbol used to denote each set
also represent its cardinality (e.g., if there are ten features, let
\\F\\ represent the set of ten features and also the number ten).

\$\$ \mathrm{Maximize} \space (\sum\_{b = 0}^{B} L_b R_b) +
\sum\_{f}^{F} (1 - E_f) W_f \space \mathrm{(eqn \space 1a)} \\
\mathrm{Subject \space to} \space \sum\_{i = 0}^{I} C_i \leq m \space
\mathrm{(eqn \space 1b)} \\ R_b = 1 - \prod\_{f = 0}^{F} ifelse(T\_{bf}
== 1, \space E_f, \space 1) \space \forall \space b \in B \space
\mathrm{(eqn \space 1c)} \\ E_f = 1 - \sum\_{j = 0}^{J} Z\_{fj} Q\_{fj}
\space \forall \space f \in F \space \mathrm{(eqn \space 1d)} \\ Z\_{fj}
\leq Y\_{j} \space \forall \space j \in J \space \mathrm{(eqn \space
1e)} \\ \sum\_{j = 0}^{J} Z\_{fj} \times \mathrm{ceil}(Q\_{fj}) = 1
\space \forall \space f \in F \space \mathrm{(eqn \space 1f)} \\ A\_{ij}
Y\_{j} \leq X\_{i} \space \forall \space i \in I, j \in J \space
\mathrm{(eqn \space 1g)} \\ E\_{f}, R\_{b} \geq 0, E\_{f}, R\_{b} \leq 1
\space \forall \space b \in B \space f \in F \space \mathrm{(eqn \space
1h)} \\ X\_{i}, Y\_{j}, Z\_{fj} \in \\0, 1\\ \space \forall \space i \in
I, j \in J, f \in F \space \mathrm{(eqn \space 1i)} \$\$

The objective (eqn 1a) is to maximize the expected phylogenetic
diversity (Faith 2008) plus the probability each feature will remain
multiplied by their weights (noting that the feature weights default to
zero). Constraint (eqn 1b) limits the maximum expenditure (i.e. ensures
that the cost of the funded actions do not exceed the budget).
Constraints (eqn 1c) calculate the probability that each branch
(including tips that correspond to a single feature) will go extinct
according to the probability that the features which share a given
branch will go extinct. Constraints (eqn 1d) calculate the probability
that each feature will go extinct according to their allocated project.
Constraints (eqn 1e) ensure that feature can only be allocated to
projects that have all of their actions funded. Constraints (eqn 1f)
state that each feature can only be allocated to a single project.
Constraints (eqn 1g) ensure that a project cannot be funded unless all
of its actions are funded. Constraints (eqns 1h) ensure that the
probability variables (\\E_f\\) are bounded between zero and one.
Constraints (eqns 1i) ensure that the action funding (\\X_i\\), project
funding (\\Y_j\\), and project allocation (\\Z\_{fj}\\) variables are
binary.

Although this formulation is a mixed integer quadratically constrained
programming problem (due to eqn 1c), it can be approximated using linear
terms and then solved using commercial mixed integer programming
solvers. This can be achieved by substituting the product of the feature
extinction probabilities (eqn 1c) with the sum of the log feature
extinction probabilities and using piecewise linear approximations
(described in Hillier & Price 2005 pp. 390–392) to approximate the
exponent of this term.

## References

Bennett JR, Elliott G, Mellish B, Joseph LN, Tulloch AI, Probert WJ, Di
Fonzo MMI, Monks JM, Possingham HP & Maloney R (2014) Balancing
phylogenetic diversity and species numbers in conservation
prioritization, using a case study of threatened species in New Zealand.
*Biological Conservation*, **174**: 47–54.

Faith DP (2008) Threatened species and the potential loss of
phylogenetic diversity: conservation scenarios based on estimated
extinction probabilities and phylogenetic risk analysis. *Conservation
Biology*, **22**: 1461–1470.

Hillier FS & Price CC (2005) *International series in operations
research & management science*. Springer.

## See also

Other objectives:
[`add_max_richness_objective()`](https://prioritizr.github.io/oppr/reference/add_max_richness_objective.md),
[`add_max_targets_met_objective()`](https://prioritizr.github.io/oppr/reference/add_max_targets_met_objective.md),
[`add_max_wtd_sum_objective()`](https://prioritizr.github.io/oppr/reference/add_max_wtd_sum_objective.md),
[`add_min_set_objective()`](https://prioritizr.github.io/oppr/reference/add_min_set_objective.md)

## Examples

``` r
# \dontrun{
# load data
data(sim_projects, sim_features, sim_actions, sim_tree)

# plot tree
plot(sim_tree)


# build problem with maximum phylogenetic diversity objective and $200 budget
p1 <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_max_phylo_div_objective(budget = 200, tree = sim_tree) %>%
  add_binary_decisions()

# solve problem
s1 <- solve(p1)
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
#> Optimize a model with 30 rows, 30 columns and 83 nonzeros (Max)
#> Model fingerprint: 0x5edd1d39
#> Model has 5 linear objective coefficients
#> Model has 3 piecewise-linear objective terms
#> Variable types: 8 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [2e-01, 2e+00]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#>   PWLObj x range   [6e-01, 5e+00]
#>   PWLObj obj range [5e-03, 1e+00]
#> Found heuristic solution: objective 1.7229965
#> Presolve removed 16 rows and 12 columns
#> Presolve time: 0.00s
#> Presolved: 17 rows, 268 columns, 290 nonzeros
#> Variable types: 253 continuous, 15 integer (15 binary)
#> Root relaxation presolved: 14 rows, 265 columns, 284 nonzeros
#> 
#> 
#> Root relaxation: objective 2.638320e+00, 22 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    2.63832    0    6    1.72300    2.63832  53.1%     -    0s
#> H    0     0                       1.9903437    2.63832  32.6%     -    0s
#> H    0     0                       2.1724129    2.63832  21.4%     -    0s
#> H    0     0                       2.5726400    2.63832  2.55%     -    0s
#>      0     0    2.59314    0    6    2.57264    2.59314  0.80%     -    0s
#>      0     0     cutoff    0         2.57264    2.57264  0.00%     -    0s
#> 
#> Cutting planes:
#>   MIR: 1
#> 
#> Explored 1 nodes (32 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 2.57264 
#> No other solutions better than 2.57264
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 2.572640035634e+00, best bound 2.572640035634e+00, gap 0.0000%

# print solution
print(s1)
#> # A tibble: 1 × 21
#>   solution status   cost   obj F1_action F2_action F3_action F4_action F5_action
#>      <int> <chr>   <dbl> <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#> 1        1 OPTIMAL  194.  2.57         1         0         0         0         1
#> # ℹ 12 more variables: baseline_action <dbl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>

# plot solution
plot(p1, s1)


# build another problem that includes feature weights
p2 <- p1 %>% add_feature_weights("weight")

# solve problem with feature weights
s2 <- solve(p2)
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
#> Optimize a model with 30 rows, 30 columns and 83 nonzeros (Max)
#> Model fingerprint: 0x71be7a4e
#> Model has 5 linear objective coefficients
#> Model has 3 piecewise-linear objective terms
#> Variable types: 8 continuous, 22 integer (22 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-02, 1e+02]
#>   Objective range  [4e-01, 3e+00]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [1e+00, 2e+02]
#>   PWLObj x range   [6e-01, 5e+00]
#>   PWLObj obj range [5e-03, 1e+00]
#> Found heuristic solution: objective 2.3884610
#> Presolve removed 16 rows and 12 columns
#> Presolve time: 0.00s
#> Presolved: 17 rows, 268 columns, 290 nonzeros
#> Variable types: 253 continuous, 15 integer (15 binary)
#> Root relaxation presolved: 14 rows, 265 columns, 284 nonzeros
#> 
#> 
#> Root relaxation: objective 4.074385e+00, 20 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0    4.07438    0    6    2.38846    4.07438  70.6%     -    0s
#> H    0     0                       2.7497080    4.07438  48.2%     -    0s
#> H    0     0                       4.0564295    4.07438  0.44%     -    0s
#> *    0     0               0       4.0567148    4.05671  0.00%     -    0s
#> 
#> Explored 1 nodes (21 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 4.05671 
#> No other solutions better than 4.05671
#> 
#> Optimal solution found (tolerance 0.00e+00)
#> Best objective 4.056714846278e+00, best bound 4.056714846278e+00, gap 0.0000%

# print solution based on feature weights
print(s2)
#> # A tibble: 1 × 21
#>   solution status   cost   obj F1_action F2_action F3_action F4_action F5_action
#>      <int> <chr>   <dbl> <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#> 1        1 OPTIMAL  199.  4.06         0         0         0         1         1
#> # ℹ 12 more variables: baseline_action <dbl>, F1_project <lgl>,
#> #   F2_project <lgl>, F3_project <lgl>, F4_project <lgl>, F5_project <lgl>,
#> #   baseline_project <lgl>, F1 <dbl>, F2 <dbl>, F3 <dbl>, F4 <dbl>, F5 <dbl>

# plot solution based on feature weights
plot(p2, s2)

# }
```
