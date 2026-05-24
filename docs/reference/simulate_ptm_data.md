# Simulate data for 'Priority threat management'

Simulate data for developing project prioritizations for a priority
threat management exercise (Carwardine *et al.* 2019). Here, data are
simulated for a pre-specified number of features, actions, and projects.
Features can benefit from multiple projects, and different projects can
share actions.

## Usage

``` r
simulate_ptm_data(
  number_projects,
  number_actions,
  number_features,
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

- number_projects:

  `numeric` number of projects. Note that this does not include the
  baseline project.

- number_actions:

  `numeric` number of actions. Note that this does not include the
  baseline action.

- number_features:

  `numeric` number of features.

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

- `"projects"`:

  A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing the data for the conservation projects. It contains the
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
  containing the data for the conservation actions. It contains the
  following columns.

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

- `"features"`:

  A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing the data for the conservation features (e.g., species). It
  contains the following columns.

  `"name"`

  :   `character` name for each feature.

  `"weight"`

  :   `numeric` weight for each feature. For each feature, this is
      calculated as the amount of time that elapsed between the present
      and the features' last common ancestor. In other words, the
      weights are calculated as the unique amount of evolutionary
      history that each feature has experienced.

- "tree":

  An [`ape::phylo()`](https://rdrr.io/pkg/ape/man/read.tree.html)
  phylogenetic tree for the features.

## Details

The simulated data set will contain one conservation project for each
features, and also a "baseline" (do nothing) project to reflect
features' persistence when their conservation project is not funded.
Each conservation project is associated with a single action, and no
conservation projects share any actions. Specifically, the data are
simulated using the following procedure.

1.  A specified number of conservation projects, features, and
    management actions are created.

2.  Cost data for each action are simulated using a normal distribution
    and the `cost_mean` and `cost_sd` arguments.

3.  A set proportion of the actions are randomly set to be locked in and
    out of the solutions using the `locked_in_proportion` and
    `locked_out_proportion` arguments.

4.  The probability of each project succeeding if its action is funded
    is simulated by drawing probabilities from a uniform distribution
    with the upper and lower bounds set as the `success_min_probability`
    and `success_max_probability` arguments.

5.  The probability of each feature persisting if various projects are
    funded and is successful is simulated by drawing probabilities from
    a uniform distribution with the upper and lower bounds set as the
    `funded_min_persistence_probability` and
    `funded_max_persistence_probability` arguments. To prevent

6.  An additional project is created which represents the "baseline" (do
    nothing) scenario. The probability of each feature persisting when
    managed under this project is simulated by drawing probabilities
    from a uniform distribution with the upper and lower bounds set as
    the `baseline_min_persistence_probability` and
    `baseline_max_persistence_probability` arguments.

7.  A phylogenetic tree is simulated for the features using
    [`ape::rcoal()`](https://rdrr.io/pkg/ape/man/rtree.html).

8.  Feature data are created from the phylogenetic tree. The weights are
    calculated as the amount of evolutionary history that has elapsed
    between each feature and its last common ancestor.

## References

Carwardine J, Martin TG, Firn J, Ponce-Reyes P, Nicol S, Reeson A,
Grantham HS, Stratford D, Kehoe L, Chades I (2019) Priority Threat
Management for biodiversity conservation: A handbook. *Journal of
Applied Ecology*, **56**: 481–490.

## See also

[`simulate_ppp_data()`](https://prioritizr.github.io/oppr/reference/simulate_ppp_data.md).

## Examples

``` r
# create a simulated data set
s <- simulate_ptm_data(
  number_projects = 6,
  number_actions = 8,
  number_features = 5,
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
#> # A tibble: 7 × 16
#>   name   success     F1      F2     F3     F4      F5 action_1 action_2 action_3
#>   <chr>    <dbl>  <dbl>   <dbl>  <dbl>  <dbl>   <dbl> <lgl>    <lgl>    <lgl>   
#> 1 proje…   0.760  0.831  0.880   0.774  0.768 NA      TRUE     TRUE     FALSE   
#> 2 proje…   0.815  0.731  0.839  NA     NA      0.527  FALSE    FALSE    TRUE    
#> 3 proje…   0.707  0.787 NA       0.636  0.647 NA      FALSE    FALSE    FALSE   
#> 4 proje…   0.768 NA     NA      NA     NA      0.798  FALSE    FALSE    TRUE    
#> 5 proje…   0.934 NA      0.694   0.651 NA     NA      TRUE     TRUE     TRUE    
#> 6 proje…   0.746  0.656 NA       0.763  0.785 NA      FALSE    FALSE    FALSE   
#> 7 basel…   1      0.323  0.0901  0.298  0.126  0.0215 FALSE    FALSE    FALSE   
#> # ℹ 6 more variables: action_4 <lgl>, action_5 <lgl>, action_6 <lgl>,
#> #   action_7 <lgl>, action_8 <lgl>, baseline_action <lgl>
#> 
#> $actions
#> # A tibble: 9 × 4
#>   name             cost locked_in locked_out
#>   <chr>           <dbl> <lgl>     <lgl>     
#> 1 action_1         87.9 FALSE     FALSE     
#> 2 action_2        101.  FALSE     FALSE     
#> 3 action_3        102.  FALSE     FALSE     
#> 4 action_4         96.9 TRUE      FALSE     
#> 5 action_5        110.  FALSE     FALSE     
#> 6 action_6        103.  FALSE     FALSE     
#> 7 action_7         98.1 FALSE     TRUE      
#> 8 action_8         99.3 FALSE     FALSE     
#> 9 baseline_action   0   FALSE     FALSE     
#> 
#> $features
#> # A tibble: 5 × 2
#>   name  weight
#>   <chr>  <dbl>
#> 1 F1    0.207 
#> 2 F2    0.207 
#> 3 F3    0.357 
#> 4 F4    0.0808
#> 5 F5    0.0808
#> 
#> $tree
#> 
#> Phylogenetic tree with 5 tips and 4 internal nodes.
#> 
#> Tip labels:
#>   F1, F2, F3, F4, F5
#> 
#> Rooted; includes branch length(s).
#> 
```
