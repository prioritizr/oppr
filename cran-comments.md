Dear CRAN maintainers,

Thank you very much for reviewing this submission. This submission aims to address the warnings and errors raised by CRAN's checks. It also contains updates to ensure compatibility with the current and upcomming version of the Matrix package.

Cheers,

Jeff

# Test environments

* [Ubuntu 20.04, R-release](https://github.com/prioritizr/oppr/actions?query=workflow%3AUbuntu)
* [Ubuntu 20.04, R-devel](https://github.com/prioritizr/oppr/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/prioritizr/oppr/actions?query=workflow%3A%22Mac+OSX%22)
* [Windows Server 2019, R-release](https://github.com/prioritizr/oppr/actions?query=workflow%3AWindows)
* [Windows Server 2019, R-devel](https://github.com/prioritizr/oppr/actions?query=workflow%3AWindows)
* Windows Server 2008 (x64), R-devel (win-builder)

# R CMD check results

0 errors | 0 warnings | 2 notes

# Notes

* checking package dependencies ... NOTE
    Package suggested but not available for checking: 'gurobi'

    **The gurobi R package is distributed with the Gurobi optimization software (http://gurobi.com). It is listed under the Enhances and Suggests fields by several R packages that are already on CRAN, such as the designmatch, DoE.MIParray, OptimalDesign, prioritizr, and raptr R packages. To help users, we have provided instructions to install the gurobi R package in the documentation for the oppr package (e.g. README file and the manual entry for the add_gurobi_solver function).**

* checking installed package size ... NOTE
    installed size is 16.9Mb
    sub-directories of 1Mb or more:
      libs  15.2Mb

    **The package makes extensive use of compiled C++ code (via the Rcpp package) to reduce run times.**
