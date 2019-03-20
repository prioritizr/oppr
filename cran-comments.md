Dear CRAN maintainers,

I sincerely apologize for submitting an updated version of the _oppr_ R package so soon after its initial release on CRAN. Prof Brian Ripley alerted me to the fact that the current version on CRAN is failing illegal memory address checks (https://cran.r-project.org/web/checks/check_results_oppr.html). This version should address these issues. I am extremely sorry for taking up more of your time.

Cheers,

Jeff

## R CMD check results

0 errors | 0 warnings | 2 note

## Notes

* checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Jeffrey O Hanson <jeffrey.hanson@uqconnect.edu.au>’

    Days since last update: 2

* checking package dependencies ... NOTE
    Package suggested but not available for checking: 'gurobi'

    **The gurobi R package is distributed with the Gurobi optimization software (http://gurobi.com). It is listed under the Enhances and Suggests fields by several R packages that are already on CRAN, such as the designmatch, DoE.MIParray, OptimalDesign, prioritizr, and raptr R packages. To help users, we have provided instructions to install the gurobi R package in the documentation for the oppr package (e.g. README file and the manual entry for the add_gurobi_solver function).**

## Test environments

* [Ubuntu 14.04.5, R-release (travis-ci)](https://travis-ci.org/prioritizr/oppr/builds)
* [Ubuntu 14.04.5, R-devel (travis-ci)](https://travis-ci.org/prioritizr/oppr/builds)
* [Mac OSX 10.9.5, R-release (travis-ci](https://travis-ci.org/prioritizr/oppr/builds)
* [Windows Server 2012 R2 (x64), R-release (appveyor)](https://ci.appveyor.com/project/jeffreyhanson/oppr)
* Windows Server 2008 (x64), R-release (win-builder)
* Windows Server 2008 (x64), R-devel (win-builder)
* [Ubuntu 18.04, R-devel, GCC ASAN](https://hub.docker.com/r/wch1/r-debug)

## Downstream dependencies

There are no packages that currently depend on this package.
