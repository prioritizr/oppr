
<!--- README.md is generated from README.Rmd. Please edit that file -->
Project Prioritization
======================

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis Build Status](https://img.shields.io/travis/prioritizr/ppr/master.svg?label=Linux%20%26%20Mac%20OSX)](https://travis-ci.org/prioritizr/ppr) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jeffreyhanson/ppr/master.svg?label=Windows)](https://ci.appveyor.com/project/jeffreyhanson/ppr) [![Coverage Status](https://codecov.io/github/prioritizr/ppr/coverage.svg?branch=master)](https://codecov.io/github/prioritizr/ppr?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ppr)](https://CRAN.R-project.org/package=ppr)

**This package is still under development and not ready for use. Please do not use this package yet.**

The *ppr R* package is decision support tool for prioritizing conservation projects. Prioritizations can be developed by maximizing expected species richness, expected phylogenetic diversity, the number of species that meet persistence targets, or identifying a set of projects that meet persistence targets for minimal cost. Constraints (e.g. lock in specific actions) and feature weights can also be specified to further customize prioritizations. After defining a project prioritization problem, solutions can be obtained using exact algorithms, heuristic algorithms, or using random processes. In particular, it is recommended to install the ['Gurobi' optimizer](https://www.gurobi.com) because it can identify optimal solutions very quickly. Finally, methods are provided for comparing different prioritizations and evaluating their benefits.

Installation
------------

The latest development version of the *ppr R* package can be installed using the following code. Please note that you will need install the *ggtree* package from Bioconductor since it is not available on [The Comprehensive R Archive Network](https://cran.r-project.org/).

``` r
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("prioritizr/ppr")
if (!require(ggtree))
  devtools::install_bioc("ggtree")
```

We also recommend installing the [Gurobi optimization suite](http://www.gurobi.com/) and [*gurobi R* package](https://www.gurobi.com/documentation/8.1/refman/r_api_overview.html) to obtain solutions very quickly. For instructions on installing these software packages, please refer to [this installation guide](https://cran.r-project.org/web/packages/prioritizr/vignettes/gurobi_installation.html).

Usage
-----

Here we will provide a short example showing how the *optimalppp R* package can be used to prioritize funding for conservation projects. To start off, we will set the seed for the random number generator to ensure you get the same results as shown here, and load the *ppr R* package.

``` r
set.seed(500)
library(ppr)
```

Now we will load some data sets that are distributed with the package. First, we will load the `sim_features` object. This table contains information on the conservation features (e.g. species). Specifically, each row corresponds to a different feature, and each column contains information associated with the features. In this table, the `"name"` column contains the name of each feature, and the `"weight"` column denotes the relative importance for each feature.

``` r
# load data
data(sim_features)

# print table
head(as.data.frame(sim_features))
```

    ##   name    weight
    ## 1   F1 0.2113582
    ## 2   F2 0.2113582
    ## 3   F3 0.2211805
    ## 4   F4 0.6296374
    ## 5   F5 1.5916703

Next, we will load the `sim_actions` object. This table stores information about the various management actions (i.e. `tibble`). Each row corresponds to a different action, and each column describes different properties associated with the actions. These actions correspond to specific management actions that have known costs. For example, they may relate to pest eradication activities in sites of conservation importance. In this table, the `"name"` column contains the name of each action, and the `"cost"` action denotes the cost of funding each project. It also contains additional columns for customizing the solutions, but we will ignore them for now. Note that the last project---the `"baseline_action"`---has a zero cost and is used subsequently to represent the baseline probability for species when no conservation actions are funded for them.

``` r
# load data
data(sim_actions)

# print table
head(as.data.frame(sim_actions))
```

    ##              name      cost locked_in locked_out
    ## 1       F1_action  94.39929     FALSE      FALSE
    ## 2       F2_action 100.99137     FALSE      FALSE
    ## 3       F3_action 103.22583      TRUE      FALSE
    ## 4       F4_action  99.24274     FALSE      FALSE
    ## 5       F5_action  99.90791     FALSE       TRUE
    ## 6 baseline_action   0.00000     FALSE      FALSE

Additionally, we will load the `sim_projects` object. This table stores information about various conservation projects. Each row corresponds to a different project, and each column describes various properties associated with the projects. These projects correspond to groups of conservation actions. For example, a conservation project may pertain to a set of conservation actions that relate to a single species or single geographic locality. In this table, the `"name"` column contains the name of each project, the `"success"` column denotes the probability of each project succeeding if it is funded, the `"F1"`--`"F5"` columns show the enhanced probability of each species persisting if the project is funded, and the `"F1_action"`--`"F5_action"` columns indicate which actions are associated with which project. Note that the last project---the `"baseline_project"`---is associated with the `"baseline_action"` action. This project has a zero cost and represents the baseline probability of each species persisting if no other project is funded. Finally, although most projects in this example directly relate to a single species, you can input projects that directly affect the persistence of multiple species.

``` r
# load data
data(sim_projects)

# print table
head(as.data.frame(sim_projects))
```

    ##               name   success        F1        F2        F3        F4
    ## 1       F1_project 0.9190985 0.7905800 0.0000000 0.0000000 0.0000000
    ## 2       F2_project 0.9232556 0.0000000 0.8881011 0.0000000 0.0000000
    ## 3       F3_project 0.8293499 0.0000000 0.0000000 0.5020887 0.0000000
    ## 4       F4_project 0.8475053 0.0000000 0.0000000 0.0000000 0.6899938
    ## 5       F5_project 0.8137868 0.0000000 0.0000000 0.0000000 0.0000000
    ## 6 baseline_project 1.0000000 0.2977965 0.2500224 0.0864612 0.2489246
    ##          F5 F1_action F2_action F3_action F4_action F5_action
    ## 1 0.0000000      TRUE     FALSE     FALSE     FALSE     FALSE
    ## 2 0.0000000     FALSE      TRUE     FALSE     FALSE     FALSE
    ## 3 0.0000000     FALSE     FALSE      TRUE     FALSE     FALSE
    ## 4 0.0000000     FALSE     FALSE     FALSE      TRUE     FALSE
    ## 5 0.6166465     FALSE     FALSE     FALSE     FALSE      TRUE
    ## 6 0.1820005     FALSE     FALSE     FALSE     FALSE     FALSE
    ##   baseline_action
    ## 1           FALSE
    ## 2           FALSE
    ## 3           FALSE
    ## 4           FALSE
    ## 5           FALSE
    ## 6            TRUE

After loading the data, we can begin formulating the project prioritization problem. Here our goal is to maximize the overall probability that each feature is expected to persist into the future (i.e. the feature richness), whilst also accounting for the relative importance of each feature and the fact that our resources are limited such that we can only spend at most $400 on funding management actions. Now, let's build a project prioritization problem object that represents our goal.

``` r
# build problem
p <- problem(projects = sim_projects, actions = sim_actions,
             features =  sim_features, project_name_column = "name",
             project_success_column = "success", action_name_column = "name",
             action_cost_column = "cost", feature_name_column = "name") %>%
     add_max_sum_persistence_objective(budget = 400) %>%
     add_feature_weights(weight = "weight") %>%
     add_binary_decisions() %>%
     add_default_solver(verbose = FALSE)

# print problem
print(p)
```

    ## Project Prioritization Problem
    ##   actions          F1_action, F2_action, F3_action, ... (6 actions)
    ##   projects         F1_project, F2_project, F3_project, ... (6 projects)
    ##   features         F1, F2, F3, ... (5 features)
    ##   action costs:    min: 0, max: 103.22583
    ##   project success: min: 0.81379, max: 1
    ##   objective:       Maximum richness objective [budget (400)]
    ##   targets:         none
    ##   weights:         min: 0.21136, max: 1.59167
    ##   decisions        Binary decision 
    ##   constraints:     <none>
    ##   solver:          Gurobi [first_feasible (0), gap (0.1), number_solutions (1), presolve (2), solution_pool_method (2), threads (1), time_limit (2147483647), time_limit (2147483647), verbose (0)]

Next, we can solve this problem to obtain a solution. By default, we will obtain the optimal solution to our problem using an exact algorithm solver (e.g. using [Gurobi](http://www.gurobi.com/) or [Rsymphony](https://cran.r-project.org/package=Rsymphony)).

``` r
# solve problem
s <- solve(p)

# print solution
head(as.data.frame(s))
```

    ##   solution  status      obj     cost F1_action F2_action F3_action
    ## 1        1 OPTIMAL 1.512928 394.5413         1         1         0
    ##   F4_action F5_action baseline_action        F1        F2        F3
    ## 1         1         1               1 0.7266208 0.8199443 0.0864612
    ##          F4        F5
    ## 1 0.5847734 0.5018188

The `s` table contains the solution and also various statistics associated with the solution. Here, each row corresponds to a different solution. Specifically, the `"solution"` column contains an identifier for the solution (which may be useful for methods that output multiple solutions), the `"obj"` column contains the objective value (i.e. the expected feature richness for this problem), the `"cost"` column stores the cost of the solution, and the `"status"` column contains information from the solver about the solution. Additionally, it contains columns for each action (`"F1_action"`, `"F2_actions"`, `"F3_actions"`, ..., `"baseline_action"`) which indicate if each action was prioritized for funding in the solution. Furthermore, it contains column for each feature (`"F1`, `"F2"`, `"F3`, ...) which indicate the probability that each feature is expected to persist into the future under each solution. Since tabular data can be difficult to understand, let's visualize how well this solution would conserve the features. Note that features which benefit from fully funded projects, excepting the baseline project, are denoted with an asterisk.

``` r
# visualize solution
plot_feature_persistence(p, s)
```

<img src="man/figures/README-readme-plot-1.png" style="display: block; margin: auto;" />

This has just been a taster of the *ppr R* package. For more information, see the [package vignette](https://prioritizr.github.io/ppr/articles/ppr.html).

Citation
--------

Please use the following citation to cite the *ppr R* package in publications:

**This package is still under development and not ready for use. Please do not use this package yet.**

Hanson JO, Schuster R, Strimas-Mackey M, Bennett J, (2019). ppr: Project Prioritization. R package version 0.0.0.1. Available at <https://github.com/prioritizr/ppr>.
