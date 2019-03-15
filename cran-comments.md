Dear CRAN maintainers,

I am pleased to submit the initial release of the _oppr_ R package.

## R CMD check results

0 errors | 0 warnings | 2 notes

## Notes

* checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Jeffrey O Hanson <jeffrey.hanson@uqconnect.edu.au>'
    New submission

    Possibly mis-spelled words in DESCRIPTION:
      Prioritizations (6:5)
      prioritizations (10:64, 16:5)

    **This package is a new submission. The words "Prioritizations" and "prioritizations" are correct and are not mis-spelled.**

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

## Downstream dependencies

There are no existing packages that depend on this package because it is not yet on CRAN.
