Dear CRAN maintainers,

I am pleased to submit the initial release of the _oppr_ R package. This R package is a decision support tool for prioritizing conservation projects.     Prioritizations can be developed by maximizing expected feature richness, expected phylogenetic diversity, the number of features that meet persistence targets, or identifying a set of projects that meet persistence targets for minimal cost. Constraints (e.g. lock in specific actions) and feature weights can also be specified to further customize prioritizations. After defining a project prioritization problem, solutions can be obtained using exact algorithms, heuristic algorithms, or random processes. In particular, it is recommended to install the 'Gurobi' optimizer  (available from <https://www.gurobi.com>) because it can identify optimal solutions very quickly. Finally, methods are provided for comparing different prioritizations and evaluating their benefits

## R CMD check results

0 errors | 0 warnings | 1 notes

## Notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jeffrey O Hanson <jeffrey.hanson@uqconnect.edu.au>'

## Test environments

* [Ubuntu 14.04.5, R-release (travis-ci)](https://travis-ci.org/prioritizr/oppr/builds)
* [Ubuntu 14.04.5, R-devel (travis-ci)](https://travis-ci.org/prioritizr/oppr/builds)
* [Mac OSX 10.9.5, R-release (travis-ci](https://travis-ci.org/prioritizr/oppr/builds)
* [Windows Server 2012 R2 (x64), R-release (appveyor)](https://ci.appveyor.com/project/jeffreyhanson/oppr)
* Windows Server 2008 (x64), R-devel (win-builder)
* Windows Server 2008 (x64), R-release (win-builder)
* Windows Server 2008 (x64), R-devel (win-builder)

## Downstream dependencies

There are no existing packages that depend on this package because it is not yet on CRAN.
