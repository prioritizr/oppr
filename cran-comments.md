Dear CRAN maintainers,

Thank you very much for reviewing this submission. In particular, this submission contains an update for the *oppr* package. Although the package was originally accepted on CRAN in 2019, it was recently archived because I was unable to address issues with the package in time. I am sorry for my delay in submitting this update, and hope that it will be possible to get this package back on CRAN. This submission contains fixes for the errors raised by CRAN's checks that resulted in its archival. It also contains fixes for the notes raised by CRAN's checks. These fixes include addressing the vignette build failure, package citation file, and documentation links.

Also, you might notice that the VignetteBuilder field of the DESCRIPTION file contains both the knitr and rmarkdown packages. Although only knitr would typically be included here, the package does not pass checks unless rmarkdown is also included under the `_R_CHECK_DEPENDS_ONLY_=true` setting. My understanding is that CRAN will sometimes run package checks under this setting and, as such, I have included rmarkdown in the VignetteBuilder field to help ensure that the package passes CRAN checks. I also note that several other packages include both knitr and rmarkdown packages the VignetteBuilder field (e.g., [BTYD](https://github.com/cran/BTYD/blob/master/DESCRIPTION#L26), [BKTR](https://github.com/cran/BKTR/blob/master/DESCRIPTION#L31), [Require](https://github.com/cran/Require/blob/master/DESCRIPTION#L42)).

Cheers,

Jeff

# Test environments

* [Ubuntu 22.04, R-release](https://github.com/prioritizr/oppr/actions?query=workflow%3AUbuntu)
* [Ubuntu 22.04, R-devel](https://github.com/prioritizr/oppr/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/prioritizr/oppr/actions?query=workflow%3A%22Mac+OSX%22)
* [Windows Server 2019, R-release](https://github.com/prioritizr/oppr/actions?query=workflow%3AWindows)
* [Windows Server 2019, R-devel](https://github.com/prioritizr/oppr/actions?query=workflow%3AWindows)
* Windows Server 2008 (x64), R-devel (win-builder)

# R CMD check results

0 errors | 0 warnings | 1 notes

# Notes

* Possibly misspelled words in DESCRIPTION:
    Prioritizations (6:5)
    al (17:15)
    et (17:12)
    prioritizations (10:64, 16:5)

     **I have manually checked these words and confirm that they are correct. In particular, the words "et al." are routinely used for in-text citations (see https://aut.ac.nz.libguides.com/APA7th/in-text). Additionally, "prioritizations" is a plural of the term "prioritization", and is a technical term for conservation plans (see https://doi.org/10.1038/35012251).**

* CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2025-06-23 as issues were not corrected
    in time.

     **I'm sorry for not correcting the issues in time, and I believe that this submission has addressed the issues that caused archival.**

* Suggests or Enhances not in mainstream repositories:
  gurobi

    **The gurobi R package is distributed with the Gurobi optimization software (http://gurobi.com). It is listed under the Enhances and Suggests fields by several R packages that are already on CRAN, such as the designmatch, DoE.MIParray, OptimalDesign, prioritizr, and raptr R packages. To help users, we have provided instructions to install the gurobi R package in the documentation for the oppr package (e.g. README file and the manual entry for the add_gurobi_solver function).**

* Found the following (possibly) invalid URLs:
    URL: https://support.gurobi.com/hc/en-us/articles/14462206790033-How-do-I-install-Gurobi-for-R
      From: man/add_gurobi_solver.Rd
            man/oppr.Rd
            inst/doc/oppr.html
      Status: 403
      Message: Forbidden
    URL: https://support.gurobi.com/hc/en-us/articles/4534161999889-How-do-I-install-Gurobi-Optimizer
      From: man/add_gurobi_solver.Rd
            man/oppr.Rd
      Status: 403
      Message: Forbidden

    **I have manually checked these URLs and can confirm that they are valid.**

# Notes on previous submissions

* Found the following (possibly) invalid DOIs:
    DOI: 10.1111/2041-210X.13264
      From: DESCRIPTION
            inst/CITATION
      Status: Service Unavailable
      Message: 503

    **This NOTE is a false positive and the DOI is valid. Specifically, the DOI pertains to a scientific article published in the peer reviewed journal Methods in Ecology and Evolution.**

* checking installed package size ...
    installed size is 12.9Mb
    sub-directories of 1Mb or more:
      libs  11.4Mb

    **The package makes extensive use of compiled C++ code (via the Rcpp package) to reduce run times.**
