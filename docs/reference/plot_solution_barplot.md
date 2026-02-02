# Plot a bar plot to visualize a project prioritization

Create a bar plot to visualize the expected outcome associated with each
feature given the projects selected for funding by a solution.

## Usage

``` r
plot_solution_barplot(
  x,
  solution,
  n = 1,
  symbol_hjust = 0.007,
  return_data = FALSE
)
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

  `integer` solution number to visualize. Since each row in the argument
  to `solutions` corresponds to a different solution, this argument
  should correspond to a row in the argument to `solutions`. Defaults to
  1.

- symbol_hjust:

  `numeric` horizontal adjustment parameter to manually align the
  asterisks and dashes in the plot. Defaults to `0.007`. Increasing this
  parameter will shift the symbols further right. Please note that this
  parameter may require some tweaking to produce visually appealing
  publication quality plots.

- return_data:

  `logical` should the underlying data used to create the plot be
  returned instead of the plot? Defaults to `FALSE`.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object. If `return_data = TRUE`, then a `data.frame` is returned.

## Details

In this plot, each bar corresponds to a different feature. The length of
each bar indicates the expected outcome for the a given feature (e.g.,
probability of persistence), and the color of each bar indicates the
weight for a given feature. Features that directly benefit from at least
a single completely funded project with a non-zero cost are depicted
with an asterisk symbol. Additionally, features that indirectly benefit
from funded projects – because they are associated with partially funded
projects that have non-zero costs and share actions with at least one
completely funded project – are depicted with an open circle symbol.

## Examples

``` r
# set seed for reproducibility
set.seed(500)

# load the ggplot2 R package to customize plots
library(ggplot2)

# load data
data(sim_projects, sim_features, sim_actions)

# build problem
p <-
  problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
  add_max_wtd_sum_objective(budget = 400) %>%
  add_feature_weights("weight") %>%
  add_binary_decisions() %>%
  add_heuristic_solver(n = 10)

# \dontrun{
# solve problem
s <- solve(p)

# plot the first solution
plot(p, s)


# plot the second solution
plot(p, s, n = 2)


# since this function returns a ggplot2 plot object, we can customize the
# appearance of the plot using standard ggplot2 commands!
# for example, we can add a title
plot(p, s) + ggtitle("solution")


# we can also obtain the raw plotting data using return_data=TRUE
plot_data <- plot(p, s, return_data = TRUE)
print(plot_data)
#> # A tibble: 5 × 4
#>   name  outcome weight status
#>   <fct>   <dbl>  <dbl> <fct> 
#> 1 F5     0.592   1.59  Funded
#> 2 F4     0.688   0.630 Funded
#> 3 F3     0.0865  0.221 NA    
#> 4 F2     0.865   0.211 Funded
#> 5 F1     0.808   0.211 Funded
# }
```
