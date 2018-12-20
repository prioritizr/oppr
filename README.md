
<!--- README.md is generated from README.Rmd. Please edit that file -->
Project Prioritization
======================

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis Build Status](https://img.shields.io/travis/prioritizr/ppr/master.svg?label=Linux%20%26%20Mac%20OSX)](https://travis-ci.org/prioritizr/ppr) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jeffreyhanson/ppr/master.svg?label=Windows)](https://ci.appveyor.com/project/jeffreyhanson/ppr) [![Coverage Status](https://codecov.io/github/prioritizr/ppr/coverage.svg?branch=master)](https://codecov.io/github/prioritizr/ppr?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ppr)](https://CRAN.R-project.org/package=ppr)

**This package is still under development and not ready for use. Please do not use this package yet.**

The *ppr R* package is a decision support tool for prioritizing conservation projects for funding. Prioritizations can be developed by maximizing expected species richness, expected phylogenetic diversity, the number of species that meet persistence targets, or identifying a set of projects that meet persistence targets for minimal cost. After defining the optimization problem, prioritizations can be generated using exact algorithms---which are guaranteed to find optimal solutions---or conventional heuristic algorithms. Finally, this package provides methods for comparing different prioritizations and evaluating their benefits.

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

Please note that you will also need to install the [Gurobi optimization suite](http://www.gurobi.com/) and [*gurobi R* package](https://www.gurobi.com/documentation/8.1/refman/r_api_overview.html). For instructions on installing these software packages, please refer to [this installation guide](https://cran.r-project.org/web/packages/prioritizr/vignettes/gurobi_installation.html).

Usage
-----

Citation
--------

Please use the following citation to cite the *ppr R* package in publications:

**This package is still under development and not ready for use. Please do not use this package yet.**

Hanson JO, Schuster R, Strimas-Mackey M, Bennett J, (2018). ppr: Project Prioritization. R package version 0.0.0.1. Available at <https://github.com/prioritizr/ppr>.
