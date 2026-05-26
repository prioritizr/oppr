# Simulated multi-objective data

Simulated data for prioritizing conservation projects based on multiple
objectives.

## Usage

``` r
data(sim_multi_actions)

data(sim_multi_projects)

data(sim_multi_features)

data(sim_multi_tree)
```

## Format

- sim_multi_projects:

  `list` of
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  objects.

- sim_multi_actions:

  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object.

- sim_multi_features:

  `list` of
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  objects.

- sim_multi_tree:

  `list` of [`ape::phylo()`](https://rdrr.io/pkg/ape/man/read.tree.html)
  objects.

## Details

The data set contains the following objects.

- `"sim_multi_projects"`:

  A `list` of three
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  objects containing data for simulated conservation projects. Each
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object corresponds to a different objective. Within each of these
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  objects, each row corresponds to a different project and each column
  contains information about the projects. These tables contains the
  following columns.

  `"name"`

  :   These columns contain the `character` name for each project.

  `"success"`

  :   These columns contain the `numeric` probability of each project
      succeeding if it is funded.

  `"F1"` ... `"F5"`

  :   These columns contain `numeric` values for each feature (i.e.,
      `"F1"`, `"F2"`, `"F3"`, `"F4"`, ..., `"F10"`) that indicate the
      probability that the feature will persist if each project is
      successfully completed. Missing values (`NA`) indicate that a
      feature does not benefit from a project being funded.

  `"A1"` ... `"A15"`

  :   These columns contain `logical` (`TRUE`/`FALSE`) values for each
      action (i.e., `"A1"`, `"A2"`, ..., `"A15"`) indicating if the
      action is associated with each project or not.

  `"baseline_action"`

  :   These columns contain `logical` (`TRUE`/`FALSE`) values for each
      project indicating if the project is the baseline project or not.

- `sim_multi_actions`:

  A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object containing data for 15 simulated actions. Each row corresponds
  to a different action and each column contains information about the
  actions. This table contains the following columns.

  `"name"`

  :   This column contains the `character` name for each action.

  `"cost"`

  :   This column contains the `numeric` cost for each action.

  `"locked_in"`

  :   This column contains `logical` values indicating if particular
      actions should be locked into the solution.

  `"locked_out"`

  :   This column contains `logical` values indicating if particular
      actions should be locked out of the solution.

- `sim_multi_features`:

  A `list` of three
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  objects containing data for ten simulated features. Each
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object corresponds to a different objective. With each
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object, each row corresponds to a different feature and each column
  contains information about the features. These tables contains the
  following columns.

  `"name"`

  :   These columns contain `character` values denoting name for each
      feature.

  `"weight"`

  :   These columns contain `numeric` values denoting weight for each
      feature.

- sim_multi_trees:

  A `list` of
  [`ape::phylo()`](https://rdrr.io/pkg/ape/man/read.tree.html)
  phylogenetic tree objects for the features associated with each of the
  three objective.

## Examples

``` r
# load data
data(sim_multi_projects)
data(sim_multi_actions)
data(sim_multi_features)
data(sim_multi_tree)

# print project data
print(sim_multi_projects)
#> $projects_obj1
#> # A tibble: 4 × 23
#>   name      success     F1     F2     F8 A1_action A2_action A3_action A4_action
#>   <chr>       <dbl>  <dbl>  <dbl>  <dbl> <lgl>     <lgl>     <lgl>     <lgl>    
#> 1 F1_proje…   0.832  0.798 NA     NA     TRUE      FALSE     TRUE      FALSE    
#> 2 F2_proje…   0.862 NA      0.693 NA     TRUE      TRUE      FALSE     FALSE    
#> 3 F8_proje…   0.964 NA     NA      0.555 TRUE      TRUE      TRUE      TRUE     
#> 4 baseline…   1      0.139  0.399  0.280 FALSE     FALSE     FALSE     FALSE    
#> # ℹ 14 more variables: A5_action <lgl>, A6_action <lgl>, A7_action <lgl>,
#> #   A8_action <lgl>, A9_action <lgl>, A10_action <lgl>, A11_action <lgl>,
#> #   A12_action <lgl>, A13_action <lgl>, A14_action <lgl>, A15_action <lgl>,
#> #   B1_action <lgl>, B2_action <lgl>, B3_action <lgl>
#> 
#> $projects_obj2
#> # A tibble: 3 × 22
#>   name  success     F3      F4 A1_action A2_action A3_action A4_action A5_action
#>   <chr>   <dbl>  <dbl>   <dbl> <lgl>     <lgl>     <lgl>     <lgl>     <lgl>    
#> 1 F3_p…   0.886  0.856 NA      TRUE      TRUE      FALSE     TRUE      TRUE     
#> 2 F4_p…   0.850 NA      0.733  TRUE      FALSE     TRUE      TRUE      TRUE     
#> 3 base…   1      0.381  0.0483 FALSE     FALSE     FALSE     FALSE     FALSE    
#> # ℹ 13 more variables: A6_action <lgl>, A7_action <lgl>, A8_action <lgl>,
#> #   A9_action <lgl>, A10_action <lgl>, A11_action <lgl>, A12_action <lgl>,
#> #   A13_action <lgl>, A14_action <lgl>, A15_action <lgl>, B1_action <lgl>,
#> #   B2_action <lgl>, B3_action <lgl>
#> 
#> $projects_obj3
#> # A tibble: 6 × 25
#>   name           success     F5     F6     F7     F9     F10 A1_action A2_action
#>   <chr>            <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <lgl>     <lgl>    
#> 1 F5_project       0.834  0.755 NA     NA     NA     NA      TRUE      TRUE     
#> 2 F6_project       0.715 NA      0.739 NA     NA     NA      FALSE     FALSE    
#> 3 F7_project       0.890 NA     NA      0.588 NA     NA      FALSE     TRUE     
#> 4 F9_project       0.799 NA     NA     NA      0.593 NA      FALSE     FALSE    
#> 5 F10_project      0.919 NA     NA     NA     NA      0.745  TRUE      FALSE    
#> 6 baseline_proj…   1      0.382  0.151  0.369  0.332  0.0665 FALSE     FALSE    
#> # ℹ 16 more variables: A3_action <lgl>, A4_action <lgl>, A5_action <lgl>,
#> #   A6_action <lgl>, A7_action <lgl>, A8_action <lgl>, A9_action <lgl>,
#> #   A10_action <lgl>, A11_action <lgl>, A12_action <lgl>, A13_action <lgl>,
#> #   A14_action <lgl>, A15_action <lgl>, B1_action <lgl>, B2_action <lgl>,
#> #   B3_action <lgl>
#> 

# print action data
print(sim_multi_actions)
#> # A tibble: 18 × 4
#>    name        cost locked_in locked_out
#>    <chr>      <dbl> <lgl>     <lgl>     
#>  1 A1_action   94.4 FALSE     FALSE     
#>  2 A2_action  101.  FALSE     FALSE     
#>  3 A3_action  103.  FALSE     FALSE     
#>  4 A4_action   99.2 FALSE     FALSE     
#>  5 A5_action   99.9 FALSE     FALSE     
#>  6 A6_action  100.  FALSE     FALSE     
#>  7 A7_action   93.9 FALSE     FALSE     
#>  8 A8_action   91.7 TRUE      FALSE     
#>  9 A9_action  100.  FALSE     FALSE     
#> 10 A10_action  93.1 FALSE     FALSE     
#> 11 A11_action  99.7 FALSE     FALSE     
#> 12 A12_action  98.6 FALSE     TRUE      
#> 13 A13_action  88.8 FALSE     FALSE     
#> 14 A14_action  98.5 FALSE     FALSE     
#> 15 A15_action 102.  FALSE     FALSE     
#> 16 B1_action    0   FALSE     FALSE     
#> 17 B2_action    0   FALSE     FALSE     
#> 18 B3_action    0   FALSE     FALSE     

# print feature data
print(sim_multi_features)
#> [[1]]
#> # A tibble: 3 × 2
#>   name  weight
#>   <chr>  <dbl>
#> 1 F1     0.298
#> 2 F2     0.298
#> 3 F8     0.529
#> 
#> [[2]]
#> # A tibble: 2 × 2
#>   name  weight
#>   <chr>  <dbl>
#> 1 F3     0.438
#> 2 F4     0.438
#> 
#> [[3]]
#> # A tibble: 5 × 2
#>   name  weight
#>   <chr>  <dbl>
#> 1 F5    0.0211
#> 2 F6    0.0211
#> 3 F7    0.120 
#> 4 F9    0.137 
#> 5 F10   0.137 
#> 

# print phylogenetic trees
print(sim_multi_tree)
#> $tree_obj1
#> 
#> Phylogenetic tree with 3 tips and 2 internal nodes.
#> 
#> Tip labels:
#>   F1, F2, F8
#> 
#> Rooted; includes branch length(s).
#> 
#> $tree_obj2
#> 
#> Phylogenetic tree with 2 tips and 1 internal node.
#> 
#> Tip labels:
#>   F3, F4
#> 
#> Rooted; includes branch length(s).
#> 
#> $tree_obj3
#> 
#> Phylogenetic tree with 5 tips and 4 internal nodes.
#> 
#> Tip labels:
#>   F5, F6, F7, F9, F10
#> 
#> Rooted; includes branch length(s).
#> 
```
