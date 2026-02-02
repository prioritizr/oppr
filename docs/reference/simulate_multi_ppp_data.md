# Simulate multi-objective data for the 'Project Prioritization Protocol'

Simulate data for developing multi-objective project prioritizations.
Here, data are simulated such that each objective has its own features,
and each feature has its own conservation project. This structure is
similar to species-based prioritizations (e.g., Bennett *et al.* 2014).

## Usage

``` r
simulate_multi_ppp_data(
  number_objectives,
  number_features,
  number_actions,
  cost_mean = 100,
  cost_sd = 5,
  success_min_probability = 0.7,
  success_max_probability = 0.99,
  funded_min_persistence_probability = 0.5,
  funded_max_persistence_probability = 0.9,
  baseline_min_persistence_probability = 0.01,
  baseline_max_persistence_probability = 0.4,
  locked_in_proportion = 0,
  locked_out_proportion = 0
)
```

## Arguments

- number_objectives:

  Number of objectives for which to simulate data.

- number_features:

  `numeric` number of features.

- number_actions:

  Number of actions for which to simulate data.

- cost_mean:

  `numeric` average cost for the actions. Defaults to `100`.

- cost_sd:

  `numeric` standard deviation in action costs. Defaults to `5`.

- success_min_probability:

  `numeric` minimum probability of the projects succeeding if they are
  funded. Defaults to `0.7`.

- success_max_probability:

  `numeric` maximum probability of the projects succeeding if they are
  funded. Defaults to `0.99`.

- funded_min_persistence_probability:

  `numeric` minimum probability of the features persisting if projects
  are funded and successful. Defaults to `0.5`.

- funded_max_persistence_probability:

  `numeric` maximum probability of the features persisting if projects
  are funded and successful. Defaults to `0.9`.

- baseline_min_persistence_probability:

  `numeric` minimum probability of the features persisting if only the
  baseline project is funded. Defaults to `0.01`.

- baseline_max_persistence_probability:

  `numeric` maximum probability of the features persisting if only the
  baseline project is funded. Defaults to `0.4`.

- locked_in_proportion:

  `numeric` of actions that are locked into the solution. Defaults to
  `0`.

- locked_out_proportion:

  `numeric` of actions that are locked into the solution. Defaults to
  `0`.

## Value

A `list` object containing the following elements.

- `"projects_obj1"`, ..., `"projects_objN"`:

  A `list` of
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing data for the conservation projects associated with each
  objective. Each element contains a data frame with the following
  following columns.

  `"name"`

  :   `character` name for each project.

  `"success"`

  :   `numeric` probability of each project succeeding if it is funded.

  `"F1"` ... `"FN"`

  :   `numeric` columns for each feature, ranging from `"F1"` to `"FN"`
      where `N` is the number of features, indicating the probability
      that each feature will persist if it is funded and successfully
      completed. Missing values (`NA`) indicate that a feature does not
      benefit from a project being funded.

  `"F1_action"` ... `"FN_action"`

  :   `logical` columns for each action, ranging from `"F1_action"` to
      `"FN_action"` where `N` is the number of actions (equal to the
      number of features in this simulated data), indicating if an
      action is associated with a project (`TRUE`) or not (`FALSE`).

  `"baseline_action"`

  :   `logical` column indicating if a project is associated with the
      baseline action (`TRUE`) or not (`FALSE`). This action is only
      associated with the baseline project.

- `"actions"`:

  A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing the data for the conservation actions across all
  objectives. It contains the following columns.

  `"name"`

  :   `character` name for each action.

  `"cost"`

  :   `numeric` cost for each action.

  `"locked_in"`

  :   `logical` indicating if certain actions should be locked into the
      solution.

  `"locked_out"`

  :   `logical` indicating if certain actions should be locked out of
      the solution.

- `"features_obj1"`, ..., `"features_objN"`:

  A `list` of
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing data for the features (e.g., species) associated with each
  objective. Each element contains a data frame with the following
  following columns.

  `"name"`

  :   `character` name for each feature.

  `"weight"`

  :   `numeric` weight for each feature. For each feature, this is
      calculated as the amount of time that elapsed between the present
      and the features' last common ancestor. In other words, the
      weights are calculated as the unique amount of evolutionary
      history that each feature has experienced.

- `"tree_obj1"`, ..., `"tree_objN"`:

  A `list` of
  [`ape::phylo()`](https://rdrr.io/pkg/ape/man/read.tree.html)
  phylogenetic tree objects for the features associated with each
  objective (separately).

## Details

The simulated data set will contain a set of projects, wherein each
project is assigned to a particular objective and is associated with a
particular feature. Although multiple projects may be assigned to the
same objective, note that each project is associated with a different
feature. Also note that that each objective is associated with a
"baseline" "baseline" (do nothing) project to reflect features'
persistence when their conservation project is not funded. Specifically,
the data are simulated using the following procedure.

1.  A set of objectives are defined (per `number_objectives`) and a set
    of features are defined (per `number_features`).

2.  Each feature is then randomly assigned to each objective. Note that
    each objective will always have at least one of feature.

3.  A set of actions (per `number_actions`) are simulated and the costs
    for these actions are simulated using a normal distribution and the
    `cost_mean` and `cost_sd` arguments. In addition to these actions, a
    set of additional baseline actions are simulated for each objective.

4.  A set of projects are created for each feature by randomly selecting
    a set of actions.

5.  A set proportion of the actions are randomly set to be locked in and
    out of the solutions using the `locked_in_proportion` and
    `locked_out_proportion` arguments.

6.  The probability of each project succeeding if its action is funded
    is simulated by drawing probabilities from a uniform distribution
    with the upper and lower bounds set as the `success_min_probability`
    and `success_max_probability` arguments.

7.  The probability of each feature persisting if its project is funded
    and is successful is simulated by drawing probabilities from a
    uniform distribution with the upper and lower bounds set as the
    `funded_min_persistence_probability` and
    `funded_max_persistence_probability` arguments.

8.  An additional project is created for each programme which represents
    the "baseline" (do nothing) scenario. The probability of each
    feature persisting when managed under this project is simulated by
    drawing probabilities from a uniform distribution with the upper and
    lower bounds set as the `baseline_min_persistence_probability` and
    `baseline_max_persistence_probability` arguments.

9.  A phylogenetic tree is simulated for the features that belong to
    each objective (separately) using
    [`ape::rcoal()`](https://rdrr.io/pkg/ape/man/rtree.html).

10. Feature data are created from the phylogenetic tree. The weights are
    calculated as the amount of evolutionary history that has elapsed
    between each feature and its last common ancestor.

## References

Bennett JR, Elliott G, Mellish B, Joseph LN, Tulloch AI, Probert WJ, ...
& Maloney R (2014) Balancing phylogenetic diversity and species numbers
in conservation prioritization, using a case study of threatened species
in New Zealand. *Biological Conservation*, **174**: 47–54.

## See also

[`simulate_ptm_data()`](https://prioritizr.github.io/oppr/reference/simulate_ptm_data.md).

## Examples

``` r
# create a simulated data set
s <- simulate_multi_ppp_data(
  number_objectives = 3,
  number_features = 7,
  number_actions = 5,
  cost_mean = 100,
  cost_sd = 5,
  success_min_probability = 0.7,
  success_max_probability = 0.99,
  funded_min_persistence_probability = 0.5,
  funded_max_persistence_probability = 0.9,
  baseline_min_persistence_probability = 0.01,
  baseline_max_persistence_probability = 0.4,
  locked_in_proportion = 0.01,
  locked_out_proportion = 0.01
)

# print data set
print(s)
#> $projects
#> $projects$projects_obj1
#> # A tibble: 3 × 12
#>   name  success     F1      F2 A1_action A2_action A3_action A4_action A5_action
#>   <chr>   <dbl>  <dbl>   <dbl> <lgl>     <lgl>     <lgl>     <lgl>     <lgl>    
#> 1 F1_p…   0.769  0.593 NA      TRUE      FALSE     FALSE     TRUE      FALSE    
#> 2 F2_p…   0.769 NA      0.896  TRUE      TRUE      TRUE      TRUE      TRUE     
#> 3 base…   1      0.192  0.0370 FALSE     FALSE     FALSE     FALSE     FALSE    
#> # ℹ 3 more variables: B1_action <lgl>, B2_action <lgl>, B3_action <lgl>
#> 
#> $projects$projects_obj2
#> # A tibble: 4 × 13
#>   name    success      F3      F4     F7 A1_action A2_action A3_action A4_action
#>   <chr>     <dbl>   <dbl>   <dbl>  <dbl> <lgl>     <lgl>     <lgl>     <lgl>    
#> 1 F3_pro…   0.813  0.580  NA      NA     FALSE     FALSE     FALSE     TRUE     
#> 2 F4_pro…   0.768 NA       0.635  NA     FALSE     TRUE      TRUE      FALSE    
#> 3 F7_pro…   0.750 NA      NA       0.533 TRUE      FALSE     FALSE     TRUE     
#> 4 baseli…   1      0.0324  0.0143  0.349 FALSE     FALSE     FALSE     FALSE    
#> # ℹ 4 more variables: A5_action <lgl>, B1_action <lgl>, B2_action <lgl>,
#> #   B3_action <lgl>
#> 
#> $projects$projects_obj3
#> # A tibble: 3 × 12
#>   name   success     F5     F6 A1_action A2_action A3_action A4_action A5_action
#>   <chr>    <dbl>  <dbl>  <dbl> <lgl>     <lgl>     <lgl>     <lgl>     <lgl>    
#> 1 F5_pr…   0.899  0.765 NA     FALSE     FALSE     FALSE     FALSE     TRUE     
#> 2 F6_pr…   0.867 NA      0.743 TRUE      TRUE      FALSE     TRUE      TRUE     
#> 3 basel…   1      0.323  0.141 FALSE     FALSE     FALSE     FALSE     FALSE    
#> # ℹ 3 more variables: B1_action <lgl>, B2_action <lgl>, B3_action <lgl>
#> 
#> 
#> $actions
#> # A tibble: 8 × 4
#>   name       cost locked_in locked_out
#>   <chr>     <dbl> <lgl>     <lgl>     
#> 1 A1_action 101.  TRUE      FALSE     
#> 2 A2_action  92.9 FALSE     FALSE     
#> 3 A3_action  91.1 FALSE     FALSE     
#> 4 A4_action  96.9 FALSE     FALSE     
#> 5 A5_action 103.  FALSE     FALSE     
#> 6 B1_action   0   FALSE     FALSE     
#> 7 B2_action   0   FALSE     TRUE      
#> 8 B3_action   0   FALSE     FALSE     
#> 
#> $features
#> $features[[1]]
#> # A tibble: 2 × 2
#>   name  weight
#>   <chr>  <dbl>
#> 1 F1      3.08
#> 2 F2      3.08
#> 
#> $features[[2]]
#> # A tibble: 3 × 2
#>   name  weight
#>   <chr>  <dbl>
#> 1 F3     0.648
#> 2 F4     0.648
#> 3 F7     2.03 
#> 
#> $features[[3]]
#> # A tibble: 2 × 2
#>   name  weight
#>   <chr>  <dbl>
#> 1 F5     0.178
#> 2 F6     0.178
#> 
#> 
#> $tree
#> $tree$tree_obj1
#> 
#> Phylogenetic tree with 2 tips and 1 internal node.
#> 
#> Tip labels:
#>   F1, F2
#> 
#> Rooted; includes branch length(s).
#> 
#> $tree$tree_obj2
#> 
#> Phylogenetic tree with 3 tips and 2 internal nodes.
#> 
#> Tip labels:
#>   F3, F4, F7
#> 
#> Rooted; includes branch length(s).
#> 
#> $tree$tree_obj3
#> 
#> Phylogenetic tree with 2 tips and 1 internal node.
#> 
#> Tip labels:
#>   F5, F6
#> 
#> Rooted; includes branch length(s).
#> 
#> 
```
