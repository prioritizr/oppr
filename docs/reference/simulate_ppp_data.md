# Simulate data for the 'Project Prioritization Protocol'

Simulate data for developing project prioritizations. Here, data are
simulated such that each feature has its own conservation project,
similar to species-based prioritizations (e.g., Bennett *et al.* 2014).

## Usage

``` r
simulate_ppp_data(
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

1.  A conservation project is created for each feature, and each project
    is associated with its own single action.

2.  Cost data for each action are simulated using a normal distribution
    and the `cost_mean` and `cost_sd` arguments.

3.  A set proportion of the actions are randomly set to be locked in and
    out of the solutions using the `locked_in_proportion` and
    `locked_out_proportion` arguments.

4.  The probability of each project succeeding if its action is funded
    is simulated by drawing probabilities from a uniform distribution
    with the upper and lower bounds set as the `success_min_probability`
    and `success_max_probability` arguments.

5.  The probability of each feature persisting if its project is funded
    and is successful is simulated by drawing probabilities from a
    uniform distribution with the upper and lower bounds set as the
    `funded_min_persistence_probability` and
    `funded_max_persistence_probability` arguments.

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

Bennett JR, Elliott G, Mellish B, Joseph LN, Tulloch AI, Probert WJ, ...
& Maloney R (2014) Balancing phylogenetic diversity and species numbers
in conservation prioritization, using a case study of threatened species
in New Zealand. *Biological Conservation*, **174**: 47–54.

## See also

[`simulate_ptm_data()`](https://prioritizr.github.io/oppr/reference/simulate_ptm_data.md).

## Examples

``` r
# create a simulated data set
s <- simulate_ppp_data(
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
#> # A tibble: 6 × 13
#>   name          success      F1     F2     F3     F4      F5 F1_action F2_action
#>   <chr>           <dbl>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <lgl>     <lgl>    
#> 1 F1_project      0.779  0.875  NA     NA     NA     NA      TRUE      FALSE    
#> 2 F2_project      0.898 NA       0.772 NA     NA     NA      FALSE     TRUE     
#> 3 F3_project      0.787 NA      NA      0.806 NA     NA      FALSE     FALSE    
#> 4 F4_project      0.886 NA      NA     NA      0.507 NA      FALSE     FALSE    
#> 5 F5_project      0.933 NA      NA     NA     NA      0.579  FALSE     FALSE    
#> 6 baseline_pro…   1      0.0692  0.321  0.376  0.303  0.0963 FALSE     FALSE    
#> # ℹ 4 more variables: F3_action <lgl>, F4_action <lgl>, F5_action <lgl>,
#> #   baseline_action <lgl>
#> 
#> $actions
#> # A tibble: 6 × 4
#>   name             cost locked_in locked_out
#>   <chr>           <dbl> <lgl>     <lgl>     
#> 1 F1_action       111.  FALSE     TRUE      
#> 2 F2_action        96.9 TRUE      FALSE     
#> 3 F3_action       105.  FALSE     FALSE     
#> 4 F4_action       102.  FALSE     FALSE     
#> 5 F5_action        99.3 FALSE     FALSE     
#> 6 baseline_action   0   FALSE     FALSE     
#> 
#> $features
#> # A tibble: 5 × 2
#>   name  weight
#>   <chr>  <dbl>
#> 1 F1    0.0727
#> 2 F2    0.0727
#> 3 F3    0.122 
#> 4 F4    0.150 
#> 5 F5    1.50  
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
