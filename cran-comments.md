# Test environments

* [Ubuntu 18.04, R-release](https://github.com/prioritizr/wdpar/actions?query=workflow%3AUbuntu)
* [Ubuntu 18.04, R-devel](https://github.com/prioritizr/wdpar/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/prioritizr/wdpar/actions?query=workflow%3A%22Mac+OSX%22)
* [Windows Server 2019, R-release](https://github.com/prioritizr/wdpar/actions?query=workflow%3AWindows)
* [Windows Server 2019, R-devel](https://github.com/prioritizr/wdpar/actions?query=workflow%3AWindows)
* Windows Server 2008 (x64), R-devel (win-builder)

# R CMD check results

0 errors | 0 warnings | 0 note

# Notes

* checking package dependencies ... NOTE
    Package suggested but not available for checking: 'gurobi'

    **The gurobi R package is distributed with the Gurobi optimization software (http://gurobi.com). It is listed under the Enhances and Suggests fields by several R packages that are already on CRAN, such as the designmatch, DoE.MIParray, OptimalDesign, prioritizr, and raptr R packages. To help users, we have provided instructions to install the gurobi R package in the documentation for the oppr package (e.g. README file and the manual entry for the add_gurobi_solver function).**

* checking installed package size ... NOTE
    installed size is 10.7Mb
    sub-directories of 1Mb or more:
      libs   9.1Mb
