Dear CRAN maintainers,

This update pre-emptively fixes compatibility issues with the upcoming new
version of the tibble R package.

Cheers,

Jeff

## R CMD check results

0 errors | 0 warnings | 2 note

## Notes

* checking package dependencies ... NOTE
    Package suggested but not available for checking: 'gurobi'

    **The gurobi R package is distributed with the Gurobi optimization software (http://gurobi.com). It is listed under the Enhances and Suggests fields by several R packages that are already on CRAN, such as the designmatch, DoE.MIParray, OptimalDesign, prioritizr, and raptr R packages. To help users, we have provided instructions to install the gurobi R package in the documentation for the oppr package (e.g. README file and the manual entry for the add_gurobi_solver function).**

* checking installed package size ... NOTE
    installed size is 10.7Mb
    sub-directories of 1Mb or more:
      libs   9.1Mb

    **The package makes extensive use of C++ code to reduce run time.**

## Test environments

* [Ubuntu 14.04.5, R-release (travis-ci)](https://travis-ci.org/prioritizr/oppr/builds)
* [Ubuntu 14.04.5, R-devel (travis-ci)](https://travis-ci.org/prioritizr/oppr/builds)
* [Mac OSX 10.9.5, R-release (travis-ci](https://travis-ci.org/prioritizr/oppr/builds)
* [Windows Server 2012 R2 (x64), R-release (appveyor)](https://ci.appveyor.com/project/jeffreyhanson/oppr)
* Windows Server 2008 (x64), R-release (win-builder)
* Windows Server 2008 (x64), R-devel (win-builder)

## Downstream dependencies

There are no packages that currently depend on this package.
